package wms.flow.planner
package engine

import global.*
import graph.{ClosedGraph, Flow, Fork2, GraphMap, Join2, Sink, Source, Stage}
import math.{Fractionable, PiecewiseIntegrableTrajectory, StaggeredIntegrableTrajectoryFactory, given}
import queue.{FifoQueue, Heap, PriorityQueue, given}
import time.*

import scala.annotation.tailrec
import scala.collection.immutable

object RequiredPowerCalculator {
	case class StageState(demand: Either[PriorityQueue, FifoQueue])

	case class RequiredPower()

	def calc(forecast: PiecewiseIntegrableTrajectory[Heap]): RequiredPower = ???


	def calcRequiredPowerAt(
		startingInstant: Instant,
		stateAtStartingInstant: GraphMap[StageState],
		endingInstant: Instant,
		desiredBacklogAtEndingInstant: GraphMap[Duration],
		sinkByPath: Map[Path, Sink],
		sinkDownstreamDemandTrajectoryFactory: StaggeredIntegrableTrajectoryFactory[PriorityQueue]
	): GraphMap[StageState] = {


		def calcSinkDownStreamDemand(from: Instant, to: Instant)(sink: Sink): PriorityQueue = {
			val downstreamDemandTrajectory: PiecewiseIntegrableTrajectory[PriorityQueue] =
				sinkDownstreamDemandTrajectoryFactory.buildStaggeredIntegrableTrajectory { wholeGraphDownstreamQueue =>
					val sinkDownStreamQueue =
						for (priority, heap) <- wholeGraphDownstreamQueue
							yield {
								val heapPortionAssignedToThisSink = heap.filter {
									case (category, _) => sink == sinkByPath(category.path)
								}
								priority -> heapPortionAssignedToThisSink
							}
					sinkDownStreamQueue.filter { (_, h) => h.nonEmpty }
				}
			downstreamDemandTrajectory.integrate(from, to)
		}


		stateAtStartingInstant.calcUpward(
			(stage: Stage, oldState: StageState, alreadyCalculated: Map[Stage, StageState]) => {

				val desiredBacklogDuration: Duration = desiredBacklogAtEndingInstant.get(stage)

				val downstreamDemand = getDownstreamDemand(
					stage,
					stateAtStartingInstant,
					calcSinkDownStreamDemand(startingInstant, endingInstant),
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
		sinksDownstreamDemandCalculator: Sink => PriorityQueue
	): Either[PriorityQueue, FifoQueue] = {
		stage match {
			case sink: Sink =>
				Left(sinksDownstreamDemandCalculator(sink))

			case flow: Flow[?, ?] =>
				stateAtStartingInstant.get(flow.out.to.host).demand

			case join2: Join2[?, ?] =>
				stateAtStartingInstant.get(join2.out.to.host).demand

			case fork2: Fork2[?, ?] =>
				val outADemand = stateAtStartingInstant.get(fork2.outA.to.host).demand
				val outBDemand = stateAtStartingInstant.get(fork2.outB.to.host).demand
				(outADemand, outBDemand) match {
					case (Right(a), Right(b)) => Right(a ++ b)
					case (Left(a), Left(b)) => Left(a ++ b)
					case _ => throw IllegalStateException(
						s"stage=${stage.name}, outADemand=$outADemand, outBDemand=$outBDemand"
					)
				}

			case source: Source =>
				stateAtStartingInstant.get(source.out.to.host).demand

		}
	}

}
