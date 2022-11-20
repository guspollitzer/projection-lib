package wms.flow.planner
package engine

import global.*
import graph.{ClosedGraph, Flow, Fork2, GraphMap, Join2, Sink, Source, Stage}
import math.{Fractionable, PiecewiseTrajectoryAlgebra, StaggeredTrajectoryAlgebra, given}
import queue.{FifoQueue, Heap, PriorityQueue, given}
import time.*

import scala.reflect.Typeable
import scala.annotation.tailrec
import scala.collection.immutable

class RequiredPowerCalculator(val sinkDownstreamDemandTrajectoryAlgebra: PiecewiseTrajectoryAlgebra[PriorityQueue]) {

	type PiecewiseTrajectory[Q] = sinkDownstreamDemandTrajectoryAlgebra.T[Q]
	type QueueTrajectory = Either[PiecewiseTrajectory[PriorityQueue], PiecewiseTrajectory[FifoQueue]]

	def buildSinkDemandQueueTrajectory(f: PriorityQueue => PriorityQueue): QueueTrajectory = {
		val y = summon[Typeable[Long]]
		val x = summon[Typeable[scala.collection.immutable.TreeMap[Long, Int]]]
		Left(sinkDownstreamDemandTrajectoryAlgebra.buildTrajectory(stepIndex => f(sinkDownstreamDemandTrajectoryAlgebra.getPieceAt(stepIndex).wholeIntegral)))
	}

	case class StageState(
		demandTrajectory: QueueTrajectory
	)

	case class RequiredPower()

	def calc(forecast: PiecewiseTrajectory[Heap]): RequiredPower = ???


	extension (queue: PriorityQueue) {
		def filteredBySink(sink: Sink, sinkByPath: Map[Path, Sink]): PriorityQueue = {
			val sinkDownStreamQueue =
				for (priority, heap) <- queue
					yield {
						val heapPortionAssignedToThisSink = heap.filter {
							case (category, _) => sink == sinkByPath(category.path)
						}
						priority -> heapPortionAssignedToThisSink
					}
			sinkDownStreamQueue.filter { (_, h) => h.nonEmpty }
		}
	}

	def calcRequiredPowerAt(
		startingInstant: Instant,
		stateAtStartingInstant: GraphMap[StageState],
		endingInstant: Instant,
		desiredBacklogAtEndingInstant: GraphMap[Duration],
		sinkByPath: Map[Path, Sink]
	): GraphMap[StageState] = {

		def downStreamDemandTrajectoryOf(sink: Sink): PiecewiseTrajectory[PriorityQueue] = {
			sinkDownstreamDemandTrajectoryAlgebra.buildTrajectory { stepIndex =>
				sinkDownstreamDemandTrajectoryAlgebra.getPieceAt(stepIndex).wholeIntegral.filteredBySink(sink, sinkByPath)
			}
		}


		stateAtStartingInstant.calcUpward(
			(stage: Stage, oldState: StageState, alreadyCalculated: Map[Stage, StageState]) => {

				val desiredBacklogDuration: Duration = desiredBacklogAtEndingInstant.get(stage)

				val downstreamDemand = getDownstreamDemand(
					stage,
					stateAtStartingInstant,
					downStreamDemandTrajectoryOf
				)

				//				val desiredBacklogAtEndingInstant =

				oldState
			}
		)

		???
	}

	inline private def getDownstreamDemand(
		stage: Stage,
		stateAtStartingInstant: GraphMap[StageState],
		sinksDownstreamDemandTrajectoryGetter: Sink => PiecewiseTrajectory[PriorityQueue]
	): QueueTrajectory = {
		stage match {
			case sink: Sink =>
				Left(sinksDownstreamDemandTrajectoryGetter(sink))

			case source: Source =>
				stateAtStartingInstant.get(source.out.to.host).demandTrajectory

			case flow: Flow[?, ?] =>
				stateAtStartingInstant.get(flow.out.to.host).demandTrajectory

			case join2: Join2[?, ?] =>
				stateAtStartingInstant.get(join2.out.to.host).demandTrajectory

			case fork2: Fork2[?, ?] =>
				val outADemandTrajectory: QueueTrajectory = stateAtStartingInstant.get(fork2.outA.to.host).demandTrajectory
				val outBDemandTrajectory: QueueTrajectory = stateAtStartingInstant.get(fork2.outB.to.host).demandTrajectory
				(outADemandTrajectory, outBDemandTrajectory) match {
					case (Right(a), Right(b)) => Right(sinkDownstreamDemandTrajectoryAlgebra.combine[FifoQueue, FifoQueue, FifoQueue](a, b)(_ ++ _))
					case (Left(a), Left(b)) => Left(a.combineWith(b)(_ ++ _))
					case _ => throw IllegalStateException(
						s"stage=${stage.name}, outADemand=$outADemandTrajectory, outBDemand=$outBDemandTrajectory"
					)
				}
		}
	}

}
