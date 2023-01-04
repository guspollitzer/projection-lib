package wms.flow.planner
package engine

import global.*
import graph.*
import math.{Fractionable, PiecewiseAlgebra, StaggeredAlgebra, given}
import queue.{*, given}
import time.*
import util.{CaseA, CaseB, OneOf, TypeId}
import workflow.*

import scala.annotation.{tailrec, targetName}
import scala.collection.{immutable, mutable}

object RequiredPowerCalculator {
	val MAX_CONSECUTIVE_EMPTY_QUEUES = 24;
}

class RequiredPowerCalculator[PA <: PiecewiseAlgebra, CG <: ClosedGraph](val piecewiseAlgebra: PA, val closedGraph: CG) {
	import RequiredPowerCalculator.*
	import piecewiseAlgebra.*
	import closedGraph.*

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
		/** Consumes the specified quantity from the queue resulting of concatenating the prefix and the queues that constitute this piecewise queue trajectory, in order, starting from the specified index; but including only the elements that were born during or before the specified limit.
		  *
		  * @param startingIndex the index of the first piece to concatenate after the prefix.
		  * @param quantityToConsume the quantity to consume from said concatenation.
		  * @param prefix the first queue of said queues concatenation.
		  * @return the resulting consumption. */
		def consumeExistingElemsStartingAt(startingIndex: Int, quantityToConsume: Quantity, prefix: PriorityQueue, maxBornPieceIndex: Int): Consumption[PriorityQueue] = {
			@tailrec
			def loop(index: Int, concatenation: PriorityQueue, consecutiveEmptyFollowingPiecesQueues: Int): Consumption[PriorityQueue] = {
				val consumption = concatenation.consumed(quantityToConsume);
				if consumption.shortage == 0 || index >= numberOfPieces || consecutiveEmptyFollowingPiecesQueues > MAX_CONSECUTIVE_EMPTY_QUEUES then consumption
				else {
					val followingPieceQueue: PriorityQueue = for (priority, heap) <- trajectory.getWholePieceIntegralAt(index) yield {
						// Can't consume what does not exist yet. So, include only the backlog that exists at the moment of the simulated consumption.
						priority -> heap.view.filterKeys { category => category.bornPieceIndex <= maxBornPieceIndex }.toMap
					}
					// TODO aunque dudo que sea necesario, esto se podría optimizar sumando los `Consumption` en lugar de recalcularlo desde el comienzo cada vez.
					loop(index + 1, concatenation.mergedWith(followingPieceQueue), if followingPieceQueue.isEmpty then consecutiveEmptyFollowingPiecesQueues + 1 else 0);
				}
			}

			loop(startingIndex, prefix, 0)
		}
	}

	case class StageInitialState[+Q: QueueOps](backlog: Q)

	type SIS = OneOf[StageInitialState[PriorityQueue], StageInitialState[FifoQueue]]

	case class RequiredPowerAtPiece[+Q: QueueOps](backlogAtPieceEnd: Q, power: Quantity, upstreamDemand: Q, shortage: Quantity)

	type RequiredPowerTrajectory = OneOf[Trajectory[RequiredPowerAtPiece[PriorityQueue]], Trajectory[RequiredPowerAtPiece[FifoQueue]]]

	def calcRequiredPowerTrajectory(
		stateAtStartingInstant: Mapping[SIS],
		desiredBacklogAtEndingInstant: Mapping[Trajectory[DesiredBacklog]],
		maxBacklogLoad: Mapping[Int],
		sinkByPath: Map[Path, SinkN[?]],
		downstreamDemandTrajectory: Trajectory[PriorityQueue],
	): Mapping[RequiredPowerTrajectory] = {

		/** Calculates the downstream demand trajectory corresponding to the specified sink based on the global downstream trajectory and the set of process paths that feed said sink.
		  * Assumes that every path feeds one sink only. */
		def downStreamDemandTrajectoryOf(sink: SinkN[?]): Trajectory[PriorityQueue] = {
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

		stateAtStartingInstant.calcUpward[RequiredPowerTrajectory]( (
			stage: Stage,
			stageInitialState: SIS,
			alreadyCalculatedStages: Map[Stage, RequiredPowerTrajectory]
		) =>
			val trajectoryOfQueueDemandedByDownstream: QueueTrajectory = getDownstreamDemand(stage, downStreamDemandTrajectoryOf, alreadyCalculatedStages);

			// TODO break the load down by path in order to calculate the productivity more precisely.
			val trajectoryOfLoadDemandedByDownstream: Trajectory[Quantity] = trajectoryOfQueueDemandedByDownstream.toLoad;
			val desiredBacklogAtEndingInstantAtStage: Trajectory[DesiredBacklog] = desiredBacklogAtEndingInstant(stage);

			stageInitialState match {
				case CaseA(priorityStageInitialState) =>
					val requiredPowerTrajectory = buildTrajectory[StageInitialState[PriorityQueue], RequiredPowerAtPiece[PriorityQueue]](priorityStageInitialState) {
						(loopState, index, start, end) =>

							val desiredLoadAtEndingInstantAtStageAtPiece: Quantity = desiredBacklogAtEndingInstantAtStage.getPieceMeanAt(index) match {
								case Maximal =>
									maxBacklogLoad(stage);

								case Minimal(duration) =>
									scala.math.min(
										maxBacklogLoad(stage),
										trajectoryOfLoadDemandedByDownstream.integrate(end, end + duration, true)
									);
							}

							trajectoryOfQueueDemandedByDownstream match {
								case CaseA(trajectoryOfPriorityQueueDemandedByDownstream: Trajectory[PriorityQueue]) =>
									val priorityQueueDemandedByDownstream: PriorityQueue = trajectoryOfPriorityQueueDemandedByDownstream.getWholePieceIntegralAt(index);
									val loadDemandedByDownstream: Quantity = priorityQueueDemandedByDownstream.load;

									val productionNeededToSatisfyDownstreamDemand: PriorityQueue = priorityQueueDemandedByDownstream.except(loopState.backlog);
									val wayAheadBacklog: PriorityQueue = loopState.backlog.except(priorityQueueDemandedByDownstream);
									val futureDemandConsumption: Consumption[PriorityQueue] = trajectoryOfPriorityQueueDemandedByDownstream.consumeExistingElemsStartingAt(
										index + 1,
										desiredLoadAtEndingInstantAtStageAtPiece,
										wayAheadBacklog,
										index
									);
									val stageQueueAtPieceEnd = futureDemandConsumption.consumed;

									val stagePower = loadDemandedByDownstream + desiredLoadAtEndingInstantAtStageAtPiece - loopState.backlog.load;

									val upstreamQueue: PriorityQueue = productionNeededToSatisfyDownstreamDemand.mergedWith(stageQueueAtPieceEnd);

									RequiredPowerAtPiece(stageQueueAtPieceEnd, stagePower, upstreamQueue, futureDemandConsumption.shortage);


								case CaseB(trajectoryOfFifoQueueDemandedByDownstream: Trajectory[FifoQueue]) =>
									???
							}

					} { (_, requiredPowerAtPiece) => StageInitialState(requiredPowerAtPiece.upstreamDemand) }

					CaseA(requiredPowerTrajectory)

				case CaseB(fifoStageInitialState) =>
					???

			}

		)
	}

	private def getDownstreamDemand(
		stage: Stage,
		sinksDownstreamDemandTrajectoryGetter: SinkN[?] => Trajectory[PriorityQueue],
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
			case sink: SinkN[?] =>
				Some(CaseA(sinksDownstreamDemandTrajectoryGetter(sink)))

			case fork: Fork[?] =>
				fork.outs
					.map(out => getUpstreamDemandTrajectoryOf(out.to.host))
					.reduce { (outADemandTrajectory, outBDemandTrajectory) =>
						(outADemandTrajectory, outBDemandTrajectory) match {
							case (Some(CaseA(a)), Some(CaseA(b))) => Some(CaseA(a.combineWith(b)(_ ++ _)));
							case (Some(CaseB(a)), Some(CaseB(b))) => Some(CaseB(a.combineWith(b)(_ ++ _)));
							case _ => throw IllegalStateException(s"stage=${stage.name}, out demand A=$outADemandTrajectory, out demand B=$outBDemandTrajectory")
						}
					}
		}

		oQueueTrajectory.getOrElse(throw IllegalStateException(s"stage=${stage.name}, alreadyCalculatedStagesStates=$alreadyCalculatedStagesStates"))
	}

}
