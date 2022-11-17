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

	type Trajectory = Either[PiecewiseIntegrableTrajectory[PriorityQueue], PiecewiseIntegrableTrajectory[FifoQueue]]
	case class StageState(
		demandTrajectory: Trajectory
	)

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


		def downStreamDemandTrajectoryOf(sink: Sink): PiecewiseIntegrableTrajectory[PriorityQueue] = {
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
		sinksDownstreamDemandTrajectoryGetter: Sink => PiecewiseIntegrableTrajectory[PriorityQueue]
	): Either[PiecewiseIntegrableTrajectory[PriorityQueue], PiecewiseIntegrableTrajectory[FifoQueue]] = {
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
				val outADemandTrajectory: Trajectory = stateAtStartingInstant.get(fork2.outA.to.host).demandTrajectory
				val outBDemandTrajectory: Trajectory = stateAtStartingInstant.get(fork2.outB.to.host).demandTrajectory
				(outADemandTrajectory, outBDemandTrajectory) match {
					case (Right(a), Right(b)) => Right(a.combineWith(b))
					case (Left(a), Left(b)) => Left(a.combineWith(b))
					case _ => throw IllegalStateException(
						s"stage=${stage.name}, outADemand=$outADemandTrajectory, outBDemand=$outBDemandTrajectory"
					)
				}


		}
	}

}
