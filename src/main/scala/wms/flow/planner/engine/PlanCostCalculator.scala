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


object PlanCostCalculator {
	trait PowerPlan[CG <: ClosedGraph](val closedGraph: CG) {
		import closedGraph.*
		def getPowerAt(pieceIndex: PieceIndex): Mapping[Quantity]

		def getCostAt(pieceIndex: PieceIndex): Mapping[Money]
	}
}

class PlanCostCalculator[PA <: PiecewiseAlgebra, CG <: ClosedGraph](
	val piecewiseAlgebra: PA,
	val closedGraph: CG
)(
	val sinkCostParams: Map[SinkN[?], ExpirationCostAtCompletionPieceCalculator.SinkParams]
) {
	import PlanCostCalculator.*
	import closedGraph.*
	import piecewiseAlgebra.*

	case class Log(totalPowerCost: Money, powerCostByStage: Mapping[Money], accumulatedPowerCost: Money, totalExpirationCost: Money, expirationCostBySink: Map[SinkN[?], Money], accumulatedExpirationCost: Money, graphProjection: Mapping[FlowProjectionPieceCalculator.StageProjection])

	private val flowProjectionPieceCalculator = new FlowProjectionPieceCalculator[closedGraph.type](closedGraph);
	private val expirationCostAtCompletionPieceCalculator = new ExpirationCostAtCompletionPieceCalculator[closedGraph.type](closedGraph)(sinkCostParams);

	def calc(
		initialInputQueue: Mapping[Queue],
		upstreamTrajectory: Trajectory[Queue],
		powerPlan: PowerPlan[closedGraph.type],
		sourceByPath: Map[Path, Source[?]]
	): Trajectory[Log] = {
		val upstreamTools = new UpstreamTools[piecewiseAlgebra.type](piecewiseAlgebra);
		val upstreamTrajectoryFor = upstreamTools.buildTrajectoryBySourceGetter(upstreamTrajectory, sourceByPath);
		calc(initialInputQueue, upstreamTrajectoryFor, powerPlan);
	}

	def calc(
		initialInputQueue: Mapping[Queue],
		upstreamTrajectoryBySource: SourceN[?] => Trajectory[Queue],
		powerPlan: PowerPlan[closedGraph.type],
	): Trajectory[Log] = {
		var accumulatedExpirationCost: Money = ZERO_MONEY;
		var accumulatedPowerCost: Money = ZERO_MONEY;

		buildTrajectory[Mapping[Queue], Log](initialInputQueue) {
			(inputQueueAtStart: Mapping[Queue], pieceIndex: PieceIndex, start: Instant, end: Instant) =>

				val powerCostByStage = powerPlan.getCostAt(pieceIndex);
				val totalPowerCost: Money = powerCostByStage.iterator.reduce[Money]{ (a, b) => a.plus(b) }
				accumulatedPowerCost = accumulatedPowerCost.plus(totalPowerCost);

				val power = powerPlan.getPowerAt(pieceIndex);
				val graphProjection = flowProjectionPieceCalculator.calc(inputQueueAtStart, power)(source => upstreamTrajectoryBySource(source).getWholePieceIntegralAt(pieceIndex));

				val expirationCostBySink = expirationCostAtCompletionPieceCalculator.calcExpirationCost(piecewiseAlgebra.firstPieceStartingInstant, start, end, graphProjection.map { _.processed });
				val totalExpirationCost = expirationCostBySink.iterator.map(_._2).reduce[Money] { (a, b) => a.plus(b) };
				accumulatedExpirationCost = accumulatedExpirationCost.plus(totalExpirationCost);

				Log(totalPowerCost, powerCostByStage, accumulatedPowerCost, totalExpirationCost, expirationCostBySink, accumulatedExpirationCost, graphProjection)
		} {
			(_, log) => log.graphProjection.map(_.inputQueueAtEnd)
		}
	}
}