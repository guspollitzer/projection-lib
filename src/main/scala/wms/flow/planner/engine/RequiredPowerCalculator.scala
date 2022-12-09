package wms.flow.planner
package engine

import global.*
import graph.*
import math.{Fractionable, PiecewiseAlgebra, StaggeredAlgebra, given}
import queue.{*, given}
import time.*
import util.{CaseA, CaseB, OneOf, TypeId}

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

class RequiredPowerCalculator(val piecewiseAlgebra: PiecewiseAlgebra) {

	import piecewiseAlgebra.*

	type Queue = OneOf[PriorityQueue, FifoQueue]

	given TypeId[Queue] = new TypeId[Queue] {}

	extension (thisQueue: Queue) {
		def load: Quantity = thisQueue match {
			case CaseA(priorityQueue) => priorityQueue.load;
			case CaseB(fifoQueue) => fifoQueue.load;
		}

		def consumed(quantityToConsume: Quantity): OneOf[Consumption[PriorityQueue], Consumption[FifoQueue]] = thisQueue match {
			case CaseA(priorityQueue) => CaseA(priorityQueue.consumed(quantityToConsume));
			case CaseB(fifoQueue) => CaseB(fifoQueue.consumed(quantityToConsume));
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
			case CaseA(priorityTrajectory) => priorityTrajectory.map(_.load);
			case CaseB(fifoTrajectory) => fifoTrajectory.map(_.load);
		}

		def getWholePieceIntegralAt(index: Int): Queue = thisQueueTrajectory match {
			case CaseA(priorityTrajectory) => CaseA(priorityTrajectory.getWholePieceIntegralAt(index));
			case CaseB(fifoTrajectory) => CaseB(fifoTrajectory.getWholePieceIntegralAt(index));
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
				val consumption = concatenation.consumed(quantityToConsume);
				if consumption.excess == 0 || index >= numberOfPieces then consumption
				else {
					// TODO aunque dudo que sea necesario, esto se podría optimizar sumando los `Consumption` en lugar de recalcularlo desde el comienzo cada vez.
					loop(index + 1, concatenation.mergedWith(trajectory.getWholePieceIntegralAt(index)));
				}
			}

			loop(startingIndex, prefix)
		}
	}


	case class StageInitialState[+Q: QueueOps](backlog: Q)

	type SIS = OneOf[StageInitialState[PriorityQueue], StageInitialState[FifoQueue]]

	case class RequiredPowerAtPiece[+Q: QueueOps](backlogAtPieceEnd: Q, power: Quantity, upstreamDemand: Q)

	type RPaP = OneOf[RequiredPowerAtPiece[PriorityQueue],RequiredPowerAtPiece[FifoQueue]]

	given rpappq: TypeId[RequiredPowerAtPiece[PriorityQueue]] = new TypeId[RequiredPowerAtPiece[PriorityQueue]] {}
	given rpapfq: TypeId[RequiredPowerAtPiece[FifoQueue]] = new TypeId[RequiredPowerAtPiece[FifoQueue]] {}
	given TypeId[RequiredPowerAtPiece[FifoQueue]] = new TypeId[RequiredPowerAtPiece[FifoQueue]] {}

	//	given TypeId[RequiredPowerAtPiece] = new TypeId[RequiredPowerAtPiece]{}

	type RequiredPowerTrajectory = OneOf[Trajectory[RequiredPowerAtPiece[PriorityQueue]], Trajectory[RequiredPowerAtPiece[FifoQueue]]]


	def calcRequiredPowerTrajectory(
		stateAtStartingInstant: GraphMap[SIS],
		desiredBacklogAtEndingInstant: GraphMap[Trajectory[Duration]],
		maxBacklogLoad: GraphMap[Int],
		sinkByPath: Map[Path, Sink],
		downstreamDemandTrajectory: Trajectory[PriorityQueue],
	): GraphMap[RequiredPowerTrajectory] = {

		/** Calculates the downstream demand trajectory corresponding to the specified sink based on the global downstream trajectory and the set of process paths that feed said sink.
		  * Assumes that every path feeds one sink only. */
		def downStreamDemandTrajectoryOf(sink: Sink): Trajectory[PriorityQueue] = {
			for downstreamDemand <- downstreamDemandTrajectory yield {
				val sinkDownStreamQueue =
					for (priority, heap) <- downstreamDemand
						yield {
							val heapPortionAssignedToThisSink = heap.filter {
								case (category, _) => sink == sinkByPath(category.path);
							}
							priority -> heapPortionAssignedToThisSink;
						}
				sinkDownStreamQueue.filter { (_, h) => h.nonEmpty };
			}
		}

		stateAtStartingInstant.calcUpward[RequiredPowerTrajectory]((
			stage: Stage,
			stageInitialState: SIS,
			alreadyCalculated: Map[Stage, RequiredPowerTrajectory]
		) =>

			val trajectoryOfQueueDemandedByDownstream: QueueTrajectory = getDownstreamDemand(stage, downStreamDemandTrajectoryOf, alreadyCalculated);
			val trajectoryOfLoadDemandedByDownstream: Trajectory[Quantity] = trajectoryOfQueueDemandedByDownstream.toLoad;
			val desiredBacklogDurationAtStage: Trajectory[Duration] = desiredBacklogAtEndingInstant.get(stage);

			stageInitialState match {
				case CaseA(priorityStageInitialState) =>
					val requiredPowerTrajectory = buildTrajectory[StageInitialState[PriorityQueue], RequiredPowerAtPiece[PriorityQueue]](priorityStageInitialState) {
						(loopState, index, start, end) =>
							val desiredBacklogDuration: Duration = desiredBacklogDurationAtStage.getPieceMeanAt(index);
							val desiredBacklogLoad: Quantity = scala.math.min(
								maxBacklogLoad.get(stage),
								trajectoryOfLoadDemandedByDownstream.integrate(end, end + desiredBacklogDuration, true)
							);

							trajectoryOfQueueDemandedByDownstream match {
								case CaseA(trajectoryOfPriorityQueueDemandedByDownstream: Trajectory[PriorityQueue]) =>
									val priorityQueueDemandedByDownstream: PriorityQueue = trajectoryOfPriorityQueueDemandedByDownstream.getWholePieceIntegralAt(index);
									val loadDemandedByDownstream: Quantity = priorityQueueDemandedByDownstream.load;

									val productionNeededToSatisfyDownstreamDemand: PriorityQueue = priorityQueueDemandedByDownstream.except(loopState.backlog);
									val wayAheadBacklog: PriorityQueue = loopState.backlog.except(priorityQueueDemandedByDownstream);
									val futureDemandConsumption: Consumption[PriorityQueue] = trajectoryOfPriorityQueueDemandedByDownstream.consumeStartingAt(
										index + 1,
										desiredBacklogLoad,
										wayAheadBacklog
									);
									val stageQueueAtPieceEnd = futureDemandConsumption.consumed;

									val stagePower = loadDemandedByDownstream + desiredBacklogLoad - loopState.backlog.load;

									val upstreamQueue: PriorityQueue = productionNeededToSatisfyDownstreamDemand.mergedWith(stageQueueAtPieceEnd);

									RequiredPowerAtPiece(stageQueueAtPieceEnd, stagePower, upstreamQueue);


								case CaseB(trajectoryOfFifoQueueDemandedByDownstream: Trajectory[FifoQueue]) =>
									???
							}

					} (requiredPowerAtPiece => StageInitialState(requiredPowerAtPiece.upstreamDemand))

					CaseA(requiredPowerTrajectory)

				case CaseB(fifoStageInitialState) =>
					???

			}
		)
	}

	inline private def getDownstreamDemand(
		stage: Stage,
		sinksDownstreamDemandTrajectoryGetter: Sink => Trajectory[PriorityQueue],
		alreadyCalculatedStagesStates: Map[Stage, RequiredPowerTrajectory],
	): QueueTrajectory = {

		def getUpstreamDemandTrajectoryOf(stage: Stage): Option[QueueTrajectory] = {
			for stageStateTrajectory <- alreadyCalculatedStagesStates.get(stage) yield
				stageStateTrajectory match {
					case CaseA(x) => CaseA(x.map(_.upstreamDemand));
					case CaseB(x) => CaseB(x.map(_.upstreamDemand));
			}
		}

		val oQueueTrajectory: Option[QueueTrajectory] = stage match {
			case sink: Sink =>
				Some(CaseA(sinksDownstreamDemandTrajectoryGetter(sink)))

			case source: Source => getUpstreamDemandTrajectoryOf(source.out.to.host)

			case flow: Flow[?, ?] => getUpstreamDemandTrajectoryOf(flow.out.to.host)

			case join2: Join2[?, ?] => getUpstreamDemandTrajectoryOf(join2.out.to.host)

			case fork2: Fork2[?, ?] =>
				val outADemandTrajectory: Option[QueueTrajectory] = getUpstreamDemandTrajectoryOf(fork2.outA.to.host);
				val outBDemandTrajectory: Option[QueueTrajectory] = getUpstreamDemandTrajectoryOf(fork2.outB.to.host);
				(outADemandTrajectory, outBDemandTrajectory) match {
					case (Some(CaseA(a)), Some(CaseA(b))) => Some(CaseA(a.combineWith(b)(_ ++ _)));
					case (Some(CaseB(a)), Some(CaseB(b))) => Some(CaseB(a.combineWith(b)(_ ++ _)));
					case _ => throw IllegalStateException(
						s"stage=${stage.name}, outADemand=$outADemandTrajectory, outBDemand=$outBDemandTrajectory"
					)
				}
		}

		oQueueTrajectory.getOrElse(throw IllegalStateException(s"stage=${stage.name}, alreadyCalculatedStagesStates=$alreadyCalculatedStagesStates"))
	}

}
