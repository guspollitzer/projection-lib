package wms.flow.planner
package engine

import global.*
import graph.*
import math.*
import queue.{*, given}
import resource.*
import time.*
import util.*
import workflow.*


class PlanCostCalculator[PA <: PiecewiseAlgebra, CG <: ClosedGraph](
	val piecewiseAlgebra: PA,
	val closedGraph: CG
)(
	val sinkCostParams: Map[SinkN[?], PieceCostCalculator.SinkParams]
) {
	import closedGraph.*
	import piecewiseAlgebra.*

	case class Log(pieceCost: Money, accumulatedCost: Money, backlogLog: Mapping[PieceBacklogCalculator.CalculatedBacklog])

	private val pieceBacklogCalculator = new PieceBacklogCalculator[closedGraph.type](closedGraph);
	private val pieceCostCalculator = new PieceCostCalculator[closedGraph.type](closedGraph)(sinkCostParams);

	def calc(
		initialBacklog: Mapping[Queue],
		upstreamTrajectory: Trajectory[Queue],
		powerTrajectory: Trajectory[Mapping[Quantity]],
		desiredBacklogAtEndingInstant: Mapping[Trajectory[DesiredBacklog]],
		sourceByPath: Map[Path, Source[?]]
	): Trajectory[Log] = {
		val upstreamTools = new UpstreamTools[piecewiseAlgebra.type](piecewiseAlgebra);
		val upstreamTrajectoryFor = upstreamTools.buildTrajectoryBySourceGetter(upstreamTrajectory, sourceByPath);
		calc(initialBacklog, upstreamTrajectoryFor, powerTrajectory, desiredBacklogAtEndingInstant);
	}

	def calc(
		initialBacklog: Mapping[Queue],
		upstreamTrajectoryBySource: SourceN[?] => Trajectory[Queue],
		powerTrajectory: Trajectory[Mapping[Quantity]],
		desiredBacklogAtEndingInstant: Mapping[Trajectory[DesiredBacklog]],
	): Trajectory[Log] = {

		buildTrajectory[Mapping[Queue], Log](initialBacklog) {
			(backlogAtStart: Mapping[Queue], pieceIndex: Int, start: Instant, end: Instant) =>

				val power = powerTrajectory.getWholePieceIntegralAt(pieceIndex);

				val backlogLog = pieceBacklogCalculator.calc(backlogAtStart, power)(source => upstreamTrajectoryBySource(source).getWholePieceIntegralAt(pieceIndex));

				val graphInfo = combine3Mappings(backlogAtStart, backlogLog, desiredBacklogAtEndingInstant) {
					(backlogAtStartAtStage, backlogLogAtStage, desiredBacklogAtEndingInstantAtStage) =>
						PieceCostCalculator.StageInfo(
							backlogAtStartAtStage,
							backlogLogAtStage.processed,
							backlogLogAtStage.backlogAtEnd,
							backlogLogAtStage.shortage,
							desiredBacklogAtEndingInstantAtStage.getWholePieceIntegralAt(pieceIndex)
						)
				}
				val cost = pieceCostCalculator.calcBacklogCost(piecewiseAlgebra.firstPieceStartingInstant, start, end, graphInfo);

				Log(cost, ZERO_MONEY /*TODO*/, backlogLog)
		} {
			log => log.backlogLog.map(_.backlogAtEnd)
		}
	}
}