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
import ExpirationCostAtCompletionPieceCalculator.ExpirationCost
import FlowProjectionPieceCalculator.StageProjection


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

	case class ResultAtPiece(
		totalPowerCost: Money,
		powerCostByStage: Mapping[Money],
		accumulatedPowerCost: Money,
		expirationCost: ExpirationCost,
		accumulatedExpirationCost: Money,
		graphProjection: Mapping[FlowProjectionPieceCalculator.StageProjection]
	)

	private val flowProjectionPieceCalculator = new FlowProjectionPieceCalculator[closedGraph.type](closedGraph);
	private val expirationCostAtCompletionPieceCalculator = new ExpirationCostAtCompletionPieceCalculator[closedGraph.type](closedGraph)(sinkCostParams);

	def calc(
		initialInputQueue: Mapping[Queue],
		upstreamTrajectory: Trajectory[Queue],
		powerPlan: PowerPlan[closedGraph.type],
		sourceByPath: Map[Path, Source[?]]
	): Trajectory[ResultAtPiece] = {
		val upstreamTools = new UpstreamTools[piecewiseAlgebra.type](piecewiseAlgebra);
		val upstreamTrajectoryFor = upstreamTools.buildTrajectoryBySourceGetter(upstreamTrajectory, sourceByPath);
		calc(initialInputQueue, upstreamTrajectoryFor, powerPlan);
	}

	def calc(
		initialInputQueue: Mapping[Queue],
		upstreamTrajectoryBySource: SourceN[?] => Trajectory[Queue],
		powerPlan: PowerPlan[closedGraph.type],
	): Trajectory[ResultAtPiece] = {

		var previousPieceAccumulatedPowerCost: Money = ZERO_MONEY;
		var previousPieceAccumulatedExpirationCost: Money = ZERO_MONEY;

		buildTrajectory[Mapping[Queue], ResultAtPiece](initialInputQueue) {
			(inputQueueAtStart: Mapping[Queue], pieceIndex: PieceIndex, start: Instant, end: Instant) =>

				val powerCostByStage: Mapping[Money] = powerPlan.getCostAt(pieceIndex);
				val piecePowerCost: Money = powerCostByStage.iterator.reduce[Money]{ (a, b) => a.plus(b) }
				val accumulatedPowerCost: Money = previousPieceAccumulatedPowerCost.plus(piecePowerCost);
				previousPieceAccumulatedPowerCost = accumulatedPowerCost;

				val power: Mapping[Quantity] = powerPlan.getPowerAt(pieceIndex);
				val graphProjection: Mapping[StageProjection] = flowProjectionPieceCalculator.calc(pieceIndex, inputQueueAtStart, power) {
					source => upstreamTrajectoryBySource(source).getValueAt(pieceIndex)
				};

				val processed = graphProjection.map(_.processed);
				val pieceExpirationCost = expirationCostAtCompletionPieceCalculator.calcExpirationCost(piecewiseAlgebra.firstPieceStartingInstant, start, end, processed);
				val accumulatedExpirationCost: Money = previousPieceAccumulatedExpirationCost.plus(pieceExpirationCost.total);
				previousPieceAccumulatedExpirationCost = accumulatedExpirationCost;

				ResultAtPiece(piecePowerCost, powerCostByStage, accumulatedPowerCost, pieceExpirationCost, accumulatedExpirationCost, graphProjection)
		} {
			(_, resultAtPiece) => resultAtPiece.graphProjection.map(_.inputQueueAtEnd)
		}
	}
}