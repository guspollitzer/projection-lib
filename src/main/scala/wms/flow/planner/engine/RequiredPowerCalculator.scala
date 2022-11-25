package wms.flow.planner
package engine

import global.*
import graph.{ClosedGraph, Flow, Fork2, GraphMap, Join2, Sink, Source, Stage}
import math.{Fractionable, PiecewiseAlgebra, StaggeredAlgebra, given}
import queue.{FifoQueue, Heap, PriorityQueue, given}
import time.*
import util.{OneOf, CaseA, CaseB}

import util.TypeId
import scala.annotation.tailrec
import scala.collection.immutable

class RequiredPowerCalculator(val piecewiseAlgebra: PiecewiseAlgebra) {
	import piecewiseAlgebra.*

	type Backlog = OneOf[PriorityQueue, FifoQueue]
	type PiecewiseTrajectory[+Q] = piecewiseAlgebra.Trajectory[Q]
	type QueueTrajectory = OneOf[PiecewiseTrajectory[PriorityQueue], PiecewiseTrajectory[FifoQueue]]

	type CasePriority[+A, +B] = CaseA[A, B]
	type CaseFifo[+A, +B] = CaseB[A, B]


//	def buildSinkDemandQueueTrajectory(f: PriorityQueue => PriorityQueue): QueueTrajectory = {
//		CaseA(piecewiseAlgebra.buildTrajectory[PriorityQueue](stepIndex => f(piecewiseAlgebra.getPieceAt(stepIndex).wholeIntegral)))
//	}

	case class StageInitialState(backlog: Backlog)
	case class StageState(
		demandTrajectory: QueueTrajectory
	)

	case class RequiredPower()

	def calc(forecast: PiecewiseTrajectory[Heap]): RequiredPower = ???

	def calcRequiredPowerAt(
		stateAtStartingInstant: GraphMap[StageInitialState],
		desiredBacklogAtEndingInstant: GraphMap[Trajectory[Duration]],
		sinkByPath: Map[Path, Sink],
		downstreamDemandTrajectory: PiecewiseTrajectory[PriorityQueue],
	): GraphMap[StageState] = {

		def downStreamDemandTrajectoryOf(sink: Sink): PiecewiseTrajectory[PriorityQueue] = {
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


		var x = stateAtStartingInstant.calcUpward[StageState](
			(stage: Stage, initialState: StageInitialState, alreadyCalculated: Map[Stage, StageState]) => {

				val desiredBacklogDurationTrajectory: Trajectory[Duration] = desiredBacklogAtEndingInstant.get(stage)

				val downstreamDemand = getDownstreamDemand(stage, downStreamDemandTrajectoryOf, alreadyCalculated)

				downstreamDemand match
					case CaseA(priorityTrajectory: PiecewiseTrajectory[PriorityQueue]) =>

					case CaseB(fifoTrajectory: PiecewiseTrajectory[FifoQueue]) =>

				//				val desiredBacklogAtEndingInstant =

				StageState(downstreamDemand)
			}
		)

		???
	}

	inline private def getDownstreamDemand(
		stage: Stage,
		sinksDownstreamDemandTrajectoryGetter: Sink => PiecewiseTrajectory[PriorityQueue],
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
