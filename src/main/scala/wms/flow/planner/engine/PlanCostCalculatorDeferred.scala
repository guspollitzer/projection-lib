package wms.flow.planner
package engine

import excel.Sheet
import global.*
import graph.*
import math.*
import queue.{*, given}
import resource.*
import time.*
import util.*
import workflow.*
import ExpirationCostAtCompletionPieceCalculator.ExpirationCost
import FlowProjectionPieceCalculator.*

object PlanCostCalculatorDeferred {
	trait PowerPlan[CG <: ClosedGraph](val closedGraph: CG) {

		import closedGraph.*

		def getPowerAt(pieceIndex: PieceIndex): Mapping[Quantity]

		def getCostAt(pieceIndex: PieceIndex): Mapping[Money]
	}

	case class PowerAndCost(power: Quantity, cost: Money)
}

class PlanCostCalculatorDeferred[PA <: PiecewiseAlgebra, CG <: ClosedGraph](
	val piecewiseAlgebra: PA,
	val closedGraph: CG
)(
	val sinkCostParams: Map[SinkN[?], ExpirationCostAtCompletionPieceCalculator.SinkParams]
) {

	import PlanCostCalculatorDeferred.*
	import closedGraph.*
	import piecewiseAlgebra.*

	val sheet = new Sheet(9)
	import sheet.*

	case class Params(powerPlan: Trajectory[Mapping[Ref[PowerAndCost]]])

	case class Log(
		powerParams: Mapping[Ref[PowerAndCost]],
		totalPowerCost: Money,
		powerCostByStage: Mapping[Money],
		accumulatedPowerCost: Money,
		expirationCost: ExpirationCost,
		accumulatedExpirationCost: Money,
		graphProjection: Mapping[FlowProjectionPieceCalculator.StageProjection]
	)

	private val flowProjectionPieceCalculator = new FlowProjectionPieceCalculator[closedGraph.type](closedGraph);
	private val expirationCostAtCompletionPieceCalculator = new ExpirationCostAtCompletionPieceCalculator[closedGraph.type](closedGraph)(sinkCostParams);



	def buildCalcPlayground(
		initialInputQueue: Mapping[Queue],
		upstreamTrajectoryBySource: SourceN[?] => Trajectory[Queue]
	): Trajectory[Ref[Log]] = {

		// Add all the [[Sheet]] parameters. 
		val powerPlanParams: Trajectory[Mapping[Ref[PowerAndCost]]] = buildTrajectory { _ => closedGraph.createMapping { stage => sheet.addParam[PowerAndCost]() } }

		// Add all the [[Sheet]] cells - BEGIN
		
		val zeroMoneyRef: Ref[Money] = sheet.of(ZERO_MONEY);
		var previousPieceAccumulatedPowerCostRef: Ref[Money] = zeroMoneyRef;
		var previousPieceAccumulatedExpirationCostRef: Ref[Money] = zeroMoneyRef;

		buildTrajectory[Ref[Mapping[Queue]], Ref[Log]](sheet.of(initialInputQueue)) {
			(inputQueueAtStartRef: Ref[Mapping[Queue]], pieceIndex: PieceIndex, start: Instant, end: Instant) =>

				sheet.dependencyCycleWidthWatchdog();

				val powerAndCostRefByStage = powerPlanParams.getPieceMeanAt(pieceIndex);
				val piecePowerCostRef: Ref[Money] = powerAndCostRefByStage.toSeq.leftFold(zeroMoneyRef) { (acc, e) => acc.plus(e.cost) };
				val accumulatedPowerCostRef: Ref[Money] = sheet.map2(previousPieceAccumulatedPowerCostRef, piecePowerCostRef) { _ plus _ };
				previousPieceAccumulatedPowerCostRef = accumulatedPowerCostRef;

				val powerAndCostByStageOrdinalRef: Ref[Iterable[PowerAndCost]] = powerAndCostRefByStage.toSeq.sequence;
				val graphProjectionRef: Ref[Mapping[StageProjection]] = sheet.map2(inputQueueAtStartRef, powerAndCostByStageOrdinalRef) {
					(inputQueueAtStart, powerAndCostByStageOrdinal) =>
						flowProjectionPieceCalculator.calc(pieceIndex, inputQueueAtStart, closedGraph.fromIterable(powerAndCostByStageOrdinal.map(_.power))) {
							source => upstreamTrajectoryBySource(source).getWholePieceIntegralAt(pieceIndex)
						}
				};
				val expirationCostRef: Ref[ExpirationCost] = graphProjectionRef.map {
					graphProjection =>
						val processed = graphProjection.map(_.processed);
						expirationCostAtCompletionPieceCalculator.calcExpirationCost(piecewiseAlgebra.firstPieceStartingInstant, start, end, processed)
				};

				val accumulatedExpirationCostRef: Ref[Money] = sheet.map2(previousPieceAccumulatedExpirationCostRef, expirationCostRef) {
					(previousAccumulatedExpirationCost, expirationCost) => previousAccumulatedExpirationCost.plus(expirationCost.total)
				};
				previousPieceAccumulatedExpirationCostRef = accumulatedExpirationCostRef;

				sheet.map6(piecePowerCostRef, powerAndCostByStageOrdinalRef, accumulatedPowerCostRef, expirationCostRef, accumulatedExpirationCostRef, graphProjectionRef) {
					(piecePowerCost, powerAndCostByStageOrdinal, accumulatedPowerCost, expirationCost, accumulatedExpirationCost, graphProjection) =>
					Log(powerAndCostRefByStage, piecePowerCost, closedGraph.fromIterable(powerAndCostByStageOrdinal.map(_.cost)), accumulatedPowerCost, expirationCost, accumulatedExpirationCost, graphProjection)
				};
		} {
			(_, log) => log.map(_.graphProjection.map(_.inputQueueAtEnd))
		}
		// Add all the [[Sheet]] cells - END
	}
}
