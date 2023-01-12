package wms.flow.planner
package engine

import global.*
import graph.*
import math.{*, given}
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

	type QueueTrajectory = Trajectory[Queue]

	extension (trajectory: QueueTrajectory) {
		/** Consumes the specified quantity from the queue resulting of concatenating the prefix and the queues that constitute this piecewise queue trajectory, in order, starting from the specified index; but including only the elements that were born during or before the specified limit (this limit avoids consuming something that doesn't exist yet).
		  *
		  * @param startingIndex the index of the first piece to concatenate after the prefix.
		  * @param quantityToConsume the quantity to consume from said concatenation.
		  * @param prefix the first queue of said queues concatenation.
		  * @param maxBornPieceIndex the consumed queue (the one resulting of the mentioned concatenation), will not contain elements associated to a category whose `bornPieceIndex` is greater than this parameter (this limit avoids consuming something that doesn't exist yet).
		  * @return the resulting consumption. */
		def consumeExistingElemsStartingAt(startingIndex: PieceIndex, quantityToConsume: Quantity, prefix: Queue, maxBornPieceIndex: PieceIndex, atStage: Stage): Consumption[Queue] = {
			given PieceIndex = maxBornPieceIndex;
			given Stage = atStage;

			@tailrec
			def loop(index: PieceIndex, concatenation: Queue, consecutiveEmptyFollowingPiecesQueues: Int): Consumption[Queue] = {
				val consumption = concatenation.consumed(quantityToConsume);
				if consumption.shortage == 0 || index >= numberOfPieces || consecutiveEmptyFollowingPiecesQueues > MAX_CONSECUTIVE_EMPTY_QUEUES then consumption
				else {
					// Can't consume what does not exist yet. So, include only the backlog that exists at the moment of the simulated consumption.
					val followingPieceQueue: Queue = trajectory.getWholePieceIntegralAt(index).filterByCategory(_.bornPieceIndex <= maxBornPieceIndex);
					// TODO aunque dudo que sea necesario, esto se podría optimizar sumando los `Consumption` ya calculados en lugar de recalcular el `Consumption` desde el comienzo cada vez.
					loop(index + 1, concatenation.mergedWith(followingPieceQueue), if followingPieceQueue.isEmpty then consecutiveEmptyFollowingPiecesQueues + 1 else 0);
				}
			}

			loop(startingIndex, prefix, 0)
		}
	}

	case class RequiredPowerAtPiece(backlogAtPieceEnd: Queue, power: Quantity, upstreamDemand: Queue, shortage: Quantity)

	type RequiredPowerTrajectory = Trajectory[RequiredPowerAtPiece]

	/**
	  * Calculates the power required to achieve that all stages have the desired backlog at the end of each piece-interval.
	  * @param initialBacklog the number of elements that the stage has processed in advance, before they are demanded by the next stages. The backlog of a stage is the input queue of the immediate next stages (contains the elements that wait to be consumed by the next stages).
	  * @param desiredBacklogAtEndingInstant the number of elements processed in advance that should present at the end of a piece-interval, waiting to be processed during the next piece-intervals.
	  * @param backlogCapacity the backlog capacity of the stage.
	  * @param sinkByPath know the sink where every path ends.
	  * @param downstreamDemandTrajectory knows the work demanded to the sink stages.
	  * */
	def calcRequiredPowerTrajectory(
		initialBacklog: Mapping[Queue],
		desiredBacklogAtEndingInstant: Mapping[Trajectory[DesiredBacklog]],
		backlogCapacity: Mapping[Int],
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
							val heapPortionAssignedToThisSink = heap.filteredByCategory(category => sink == sinkByPath(category.path));
							priority -> heapPortionAssignedToThisSink
						}
				sinkDownStreamQueue.filter { (_, h) => h.nonEmpty };
			}
		}

		initialBacklog.calcUpward[RequiredPowerTrajectory] {
			(stage: Stage, initialBacklogAtStage: Queue, alreadyCalculatedStages: Map[Stage, RequiredPowerTrajectory]) =>
				val trajectoryOfQueueDemandedByDownstream: QueueTrajectory = getDownstreamDemand(stage, downStreamDemandTrajectoryOf, alreadyCalculatedStages);

				// TODO break the load down by path in order to calculate the productivity more precisely.
				val trajectoryOfLoadDemandedByDownstream: Trajectory[Quantity] = trajectoryOfQueueDemandedByDownstream.map(_.load);
				val desiredBacklogAtEndingInstantAtStage: Trajectory[DesiredBacklog] = desiredBacklogAtEndingInstant(stage);

				val requiredPowerTrajectory = buildTrajectory[Queue, RequiredPowerAtPiece](initialBacklogAtStage) {
					(backlogAtPieceStart, pieceIndex, start, end) =>

						val desiredLoadAtEndingInstantAtStageAtPiece: Quantity = desiredBacklogAtEndingInstantAtStage.getPieceMeanAt(pieceIndex) match {
							case Maximal =>
								backlogCapacity(stage).toFloat;

							case Minimal(duration) =>
								scala.math.min(
									backlogCapacity(stage).toFloat,
									trajectoryOfLoadDemandedByDownstream.integrate(end, end + duration, true)
								);
						}

						val queueDemandedByDownstream: Queue = trajectoryOfQueueDemandedByDownstream.getWholePieceIntegralAt(pieceIndex);
						val loadDemandedByDownstream: Quantity = queueDemandedByDownstream.load;

						// Calculate the minimum number of elements that should be processed during this piece-interval to avoid the backlog gets empty. Which is equal to the downstream demand excluding the elements that were already processed during a previous piece-interval.
						val productionNeededToSatisfyDownstreamDemand: Queue = queueDemandedByDownstream.except(backlogAtPieceStart);
						// Take, from backlog at piece start, the portion that is not demanded during this piece but during the following ones.
						val wayAheadBacklog: Queue = backlogAtPieceStart.except(queueDemandedByDownstream);
						// In order to have backlog at the end of the piece-interval, we must process during this interval elements that will be demanded in the next interval. How many? As many as indicated by the `desiredBacklogAtEndingInstant` parameter.
						// Note that, if a portion of the `wayAheadBacklog` is not demanded during the immediate next piece-interval but after that, then part of the backlog will be useless for the immediate next piece-interval. This case may happen only if the initial backlog contains elements that are demanded after the second piece-interval.
						// TODO: consider changing the behaviour such that the useless way-ahead backlog is not considered as part of the desired backlog.
						val futureDemandConsumption: Consumption[Queue] = trajectoryOfQueueDemandedByDownstream.consumeExistingElemsStartingAt(
							pieceIndex + 1,
							scala.math.max(desiredLoadAtEndingInstantAtStageAtPiece, wayAheadBacklog.load),
							wayAheadBacklog,
							pieceIndex,
							stage
						);
						val stageQueueAtPieceEnd = futureDemandConsumption.consumed;

						val stagePower = loadDemandedByDownstream + desiredLoadAtEndingInstantAtStageAtPiece - backlogAtPieceStart.load;

						val upstreamQueue: Queue = productionNeededToSatisfyDownstreamDemand.mergedWith(stageQueueAtPieceEnd);

						RequiredPowerAtPiece(stageQueueAtPieceEnd, stagePower, upstreamQueue, futureDemandConsumption.shortage);


				} { (_, requiredPowerAtPiece) => requiredPowerAtPiece.upstreamDemand };

				requiredPowerTrajectory
		}
	}

	private def getDownstreamDemand(
		stage: Stage,
		sinksDownstreamDemandTrajectoryGetter: SinkN[?] => Trajectory[PriorityQueue],
		alreadyCalculatedStagesStates: Map[Stage, RequiredPowerTrajectory],
	): QueueTrajectory = {

		def getUpstreamDemandTrajectoryOf(stage: Stage): QueueTrajectory = {
			alreadyCalculatedStagesStates.getOrElse(
				stage,
				// This happens only if the method `calcUpward` of `ClosedGraph.Mapping` has a bug.
				throw new IllegalStateException(s"stage=${stage.name}, alreadyCalculatedStagesStates=$alreadyCalculatedStagesStates")
			).map(_.upstreamDemand)
		}

		stage match {
			case sink: SinkN[?] =>
				sinksDownstreamDemandTrajectoryGetter(sink).map { priorityQueue => CaseA(priorityQueue) }

			case fork: Fork[?] =>
				fork.outs
					.map(out => getUpstreamDemandTrajectoryOf(out.to.host))
					.reduce { (outADemandTrajectory, outBDemandTrajectory) => outADemandTrajectory.combineWith(outBDemandTrajectory)(_ ++ _) }
		}
	}

}
