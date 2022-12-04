package wms.flow.planner
package engine

import global.*
import graph.*
import math.{Fractionable, PiecewiseAlgebra, StaggeredAlgebra, given}
import queue.{*, given}
import time.*
import util.{CaseA, CaseB, OneOf, TypeId}

import scala.annotation.tailrec
import scala.collection.immutable

class RequiredPowerCalculator(val piecewiseAlgebra: PiecewiseAlgebra) {
	import piecewiseAlgebra.*

	type Queue = OneOf[PriorityQueue, FifoQueue]

	extension (thisQueue: Queue) {
		def load: Quantity = thisQueue match {
			case CaseA(priorityQueue) => priorityQueue.load
			case CaseB(fifoQueue) => fifoQueue.load
		}

		def consumed(quantityToConsume: Quantity): OneOf[Consumption[PriorityQueue], Consumption[FifoQueue]] = thisQueue match {
			case CaseA(priorityQueue) => CaseA(priorityQueue.consumed(quantityToConsume))
			case CaseB(fifoQueue) => CaseB(fifoQueue.consumed(quantityToConsume))
		}

//		def mergedWith(thatQueue: Queue, minimumLoad: Quantity): Queue = {
//			(thisQueue, thatQueue) match {
//				case (CaseA(thisPriorityQueue), CaseA(thatPriorityQueue)) => CaseA(thisPriorityQueue.mergedWith(thatPriorityQueue, minimumLoad))
//				case (CaseA(thisPriorityQueue), CaseB(thatFifoQueue)) =>  CaseA(thisPriorityQueue.mergedWith(thatFifoQueue, minimumLoad))
//				case (CaseB(thisFifoQueue), CaseA(thatPriorityQueue)) =>  CaseB(thisFifoQueue.mergedWith(thatPriorityQueue, minimumLoad))
//				case (CaseB(thisFifoQueue), CaseB(thatFifoQueue)) => CaseB(thisFifoQueue.mergedWith(thatFifoQueue, minimumLoad))
//			}
//		}

//		def minus(thatQueue: Queue): Queue = {
//			(thisQueue, thatQueue) match {
//				case (CaseA(thisPriorityQueue), CaseA(thatPriorityQueue)) => thisPriorityQueue.minus(thatPriorityQueue)
//				case (CaseA(thisPriorityQueue), CaseB(thatFifoQueue)) => thisPriorityQueue.minus(thatFifoQueue)
//				case (CaseB(thisFifoQueue), CaseA(thatPriorityQueue)) => thisFifoQueue.minus(thatPriorityQueue)
//				case (CaseB(thisFifoQueue), CaseB(thatFifoQueue)) => thisFifoQueue.minus(thatFifoQueue)
//			}
//		}
	}

	type QueueTrajectory = OneOf[Trajectory[PriorityQueue], Trajectory[FifoQueue]]

	extension (thisQueueTrajectory: QueueTrajectory) {
		def toLoad: Trajectory[Quantity] = thisQueueTrajectory match {
			case CaseA(priorityTrajectory) => priorityTrajectory.map(_.load)
			case CaseB(fifoTrajectory) => fifoTrajectory.map(_.load)
		}

		def getWholePieceIntegralAt(index: Int): Queue = thisQueueTrajectory match {
			case CaseA(priorityTrajectory) => CaseA(priorityTrajectory.getWholePieceIntegralAt(index))
			case CaseB(fifoTrajectory) => CaseB(fifoTrajectory.getWholePieceIntegralAt(index))
		}
	}

	extension (trajectory: Trajectory[PriorityQueue]) {
		/** Consumes the specified quantity from the queue resulting of concatenating the prefix and the queues that constitute this piecewise queue trajectory, in order, starting from the specified index.
		  *
		  * @param startingIndex the index of the first piece to concatenate after the prefix.
		  * @param quantityToConsume the quantity to consume from said concatenation.
		  * @param prefix the first queue of said queues concatenation.
		  * @return the resulting consumption. */
		def consumeStartingAt(startingIndex: Int, quantityToConsume: Quantity, prefix: PriorityQueue): Consumption[PriorityQueue] = {
			@tailrec
			def loop(index: Int, concatenation: PriorityQueue): Consumption[PriorityQueue] = {
				val consumption = concatenation.consumed(quantityToConsume)
				if consumption.excess == 0 || index >= numberOfPieces then consumption
				else loop(index + 1, concatenation.mergedWith(trajectory.getWholePieceIntegralAt(index)))
			}
			loop(startingIndex, prefix)
		}
	}


	case class StageInitialState(backlog: Queue)
	case class StageState(demandTrajectory: QueueTrajectory)

	case class RequiredPower()

	def calcRequiredPowerAt(
		stateAtStartingInstant: GraphMap[StageInitialState],
		desiredBacklogAtEndingInstant: GraphMap[Trajectory[Duration]],
		maxBacklogLoad: GraphMap[Int],
		sinkByPath: Map[Path, Sink],
		downstreamDemandTrajectory: Trajectory[PriorityQueue],
	): GraphMap[StageState] = {

		/** Calculates the downstream demand trajectory corresponding to the specified sink based on the global downstream trajectory and the set of process paths that feed said sink.
		  * Assumes that every path feeds one sink only. */
		def downStreamDemandTrajectoryOf(sink: Sink): Trajectory[PriorityQueue] = {
			for downstreamDemand <- downstreamDemandTrajectory yield {
				val sinkDownStreamQueue =
					for (priority, heap) <- downstreamDemand
						yield {
							val heapPortionAssignedToThisSink = heap.filter {
								case (category, _) => sink == sinkByPath(category.path)
							}
							priority -> heapPortionAssignedToThisSink
						}
				sinkDownStreamQueue.filter { (_, h) => h.nonEmpty }
			}
		}

		object LoopState {
			var isFinal = false
			var stateAtPieceStart: GraphMap[StageInitialState] = stateAtStartingInstant
		}
		reduce[LoopState.type](LoopState, _.isFinal)((loopState: LoopState.type, index: Int, start: Instant, end: Instant) => {

			var x = loopState.stateAtPieceStart.calcUpward[StageState]((stage: Stage, stateAtPieceStart: StageInitialState, alreadyCalculated: Map[Stage, StageState]) => {

				val trajectoryOfQueueDemandedByDownstream: QueueTrajectory = getDownstreamDemand(stage, downStreamDemandTrajectoryOf, alreadyCalculated)
				val trajectoryOfLoadDemandedByDownstream: Trajectory[Quantity] = trajectoryOfQueueDemandedByDownstream.toLoad
				val queueDemandedByDownstream: Queue = trajectoryOfQueueDemandedByDownstream.getWholePieceIntegralAt(index)
				val loadDemandedByDownstream: Quantity = queueDemandedByDownstream.load

				val desiredBacklogDuration: Duration = desiredBacklogAtEndingInstant.get(stage).getPieceMeanAt(index)
				val desiredBacklogLoad: Quantity = scala.math.min(
					maxBacklogLoad.get(stage),
					trajectoryOfLoadDemandedByDownstream.integrate(end, end + desiredBacklogDuration)
				)

				(stateAtPieceStart.backlog, queueDemandedByDownstream) match {
					case (CaseA(backlogAtPieceStart: PriorityQueue), CaseA(priorityQueueDemandedByDownstream: PriorityQueue)) =>
						val productionNeededToSatisfyDownstreamDemand: PriorityQueue = priorityQueueDemandedByDownstream.except(backlogAtPieceStart)
						val wayAheadBacklog: PriorityQueue = backlogAtPieceStart.except(priorityQueueDemandedByDownstream)
						val futureDemandConsumption: Consumption[PriorityQueue] = trajectoryOfQueueDemandedByDownstream.a.consumeStartingAt(index + 1, desiredBacklogLoad, wayAheadBacklog)
						val stageQueueAtPieceEnd = futureDemandConsumption.consumed

						val stagePower = loadDemandedByDownstream + desiredBacklogLoad - stateAtPieceStart.backlog.load
//						val stageQueueAtPieceEnd = stateAtPieceStart.backlog.
//
//						val upstreamQueue = stateAtPieceStart.backlog.mergedWith(queueDemandedByDownstream,)


					case (CaseA(backlogAtPieceStart: PriorityQueue), CaseB(fifoQueueDemandedByDownstream: FifoQueue)) => ???
					case (CaseB(backlogAtPieceStart: FifoQueue), CaseA(priorityQueueDemandedByDownstream: PriorityQueue)) => ???
					case (CaseB(backlogAtPieceStart: FifoQueue), CaseB(fifoQueueDemandedByDownstream: FifoQueue)) => ???
					case _ => ???

				}

				StageState(trajectoryOfQueueDemandedByDownstream)
			}
			)
			???
		}
		)

		???
	}

	inline private def getDownstreamDemand(
		stage: Stage,
		sinksDownstreamDemandTrajectoryGetter: Sink => Trajectory[PriorityQueue],
		alreadyCalculatedStagesStates: Map[Stage, StageState],
	): QueueTrajectory = {
		val oQueueTrajectory: Option[QueueTrajectory] = stage match {
			case sink: Sink =>
				Some(CaseA(sinksDownstreamDemandTrajectoryGetter(sink)))

			case source: Source =>
				alreadyCalculatedStagesStates.get(source.out.to.host).map(_.demandTrajectory)

			case flow: Flow[?, ?] =>
				alreadyCalculatedStagesStates.get(flow.out.to.host).map(_.demandTrajectory)

			case join2: Join2[?, ?] =>
				alreadyCalculatedStagesStates.get(join2.out.to.host).map(_.demandTrajectory)

			case fork2: Fork2[?, ?] =>
				val outADemandTrajectory: Option[QueueTrajectory] = alreadyCalculatedStagesStates.get(fork2.outA.to.host).map(_.demandTrajectory)
				val outBDemandTrajectory: Option[QueueTrajectory] = alreadyCalculatedStagesStates.get(fork2.outB.to.host).map(_.demandTrajectory)
				(outADemandTrajectory, outBDemandTrajectory) match {
					case (Some(CaseA(a)), Some(CaseA(b))) => Some(CaseA(a.combineWith(b)(_ ++ _)))
					case (Some(CaseB(a)), Some(CaseB(b))) => Some(CaseB(a.combineWith(b)(_ ++ _)))
					case _ => throw IllegalStateException(
						s"stage=${stage.name}, outADemand=$outADemandTrajectory, outBDemand=$outBDemandTrajectory"
					)
				}
		}

		oQueueTrajectory.getOrElse( throw IllegalStateException(
				s"stage=${stage.name}, alreadyCalculatedStagesStates=$alreadyCalculatedStagesStates"
			)
		)
	}

}
