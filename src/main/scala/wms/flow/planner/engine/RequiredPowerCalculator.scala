package wms.flow.planner
package engine

import global.*
import graph.{ClosedGraph, GraphMap, Stage, Source, Sink, Flow, Fork2, Join2}
import math.PiecewiseIntegrableTrajectory
import queue.{Heap, PriorityQueue, FifoQueue}
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
		sinksDownstreamDemandTrajectory: PiecewiseIntegrableTrajectory[PriorityQueue],
		sinkByPath: Map[Path, Sink],
		desiredBacklogAtEndingInstant: GraphMap[Instant]

	): GraphMap[StageState] = {

		stateAtStartingInstant.calcUpward(
			(stage: Stage, oldState: StageState, alreadyCalculated: Map[Stage, StageState]) => {

				val desiredBacklogAtEndingInstantOfStage = desiredBacklogAtEndingInstant.get(stage)
				val sinksDownstreamDemand: PriorityQueue =
					sinksDownstreamDemandTrajectory.integrate(startingInstant, desiredBacklogAtEndingInstantOfStage)

				val downstreamDemand = getDownstreamDemand(stage, stateAtStartingInstant, sinksDownstreamDemand, sinkByPath)

				oldState
			}
		)

		???
	}

	inline private def getDownstreamDemand(
		stage: Stage,
		stateAtStartingInstant: GraphMap[StageState],
		sinksDownstreamDemand: PriorityQueue,
		sinkByPath: Map[Path, Sink]
	): Either[PriorityQueue, FifoQueue] = {
		stage match {
			case sink: Sink =>
				val sinkDemand =
					for (priority, heap) <- sinksDownstreamDemand
						yield {
							val heapPortionAssignedToThisSink = heap.filter {
								case (category, _) => sink == sinkByPath(category.path)
							}
							priority -> heapPortionAssignedToThisSink
						}
				Left(sinkDemand.filter { (_, h) => h.nonEmpty })

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
