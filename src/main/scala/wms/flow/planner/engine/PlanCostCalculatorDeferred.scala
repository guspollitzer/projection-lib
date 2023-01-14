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

//	trait PowerPlan[CG <: ClosedGraph](val closedGraph: CG) {
//
//		import closedGraph.*
//
//		def getPowerAt(pieceIndex: PieceIndex): Mapping[Quantity]
//
//		def getCostAt(pieceIndex: PieceIndex): Mapping[Money]
//	}

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

	case class ArgumentsAndResultAtPiece(powerParams: Mapping[Ref[PowerAndCost]], resultRef: Ref[ResultAtPiece])
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
		upstreamTrajectoryBySource: SourceN[?] => Trajectory[Queue],
		powerPlan: Trajectory[Mapping[PowerAndCost]] // no me gusta. Considerar volver al trait PowerPlan que comenté arriba
	): Trajectory[ResultAtPiece] = {
		val calcFunction = buildCalcFunction(initialInputQueue, upstreamTrajectoryBySource);

		val evaluationBuilder = sheet.evaluationBuilder;
		val argumentsSetterTrajectory = combine(calcFunction, powerPlan) {
			(argumentsAndResultAtPiece, powerAtPiece) =>
				combine2Mappings(argumentsAndResultAtPiece.powerParams, powerAtPiece) {
					(paramRef, argument) =>
						evaluationBuilder.setArgument(paramRef, argument);
						() // no me gusta. Considerar volver al trait PowerPlan que comenté arriba
				};
				() // no me gusta. Considerar volver al trait PowerPlan que comenté arriba
		};
		argumentsSetterTrajectory.foreach(_ => ());
		val evaluation = evaluationBuilder.complete;

		for argumentsAndResultAtPiece <- calcFunction yield {
			evaluation.get(argumentsAndResultAtPiece.resultRef)
		}
	}

	/** Builds a function that calculates the cost trajectory by means of a [[Sheet]]. */
	def buildCalcFunction(
		initialInputQueue: Mapping[Queue],
		upstreamTrajectoryBySource: SourceN[?] => Trajectory[Queue]
	): Trajectory[ArgumentsAndResultAtPiece] = {

		// Add all the [[Sheet]] parameters.
		val powerPlanParams: Trajectory[Mapping[Ref[PowerAndCost]]] = buildTrajectory { _ => closedGraph.createMapping { stage => sheet.addParam[PowerAndCost]() } }

		// Add all the [[Sheet]] cells - BEGIN

		val zeroMoneyRef: Ref[Money] = sheet.of(ZERO_MONEY);
		var previousPieceAccumulatedPowerCostRef: Ref[Money] = zeroMoneyRef;
		var previousPieceAccumulatedExpirationCostRef: Ref[Money] = zeroMoneyRef;

		buildTrajectory[Ref[Mapping[Queue]], ArgumentsAndResultAtPiece](sheet.of(initialInputQueue)) {
			(inputQueueAtStartRef: Ref[Mapping[Queue]], pieceIndex: PieceIndex, start: Instant, end: Instant) =>

				sheet.dependencyCycleWidthWatchdog();

				val powerAndCostRefByStage: Mapping[Ref[PowerAndCost]] = powerPlanParams.getValueAt(pieceIndex);
				val piecePowerCostRef: Ref[Money] = powerAndCostRefByStage.toSeq.leftFold(zeroMoneyRef) { (acc, e) => acc.plus(e.cost) };
				val accumulatedPowerCostRef: Ref[Money] = sheet.map2(previousPieceAccumulatedPowerCostRef, piecePowerCostRef) { _ plus _ };
				previousPieceAccumulatedPowerCostRef = accumulatedPowerCostRef;

				val powerAndCostByStageOrdinalRef: Ref[Iterable[PowerAndCost]] = powerAndCostRefByStage.toSeq.sequence;
				val graphProjectionRef: Ref[Mapping[StageProjection]] = sheet.map2(inputQueueAtStartRef, powerAndCostByStageOrdinalRef) {
					(inputQueueAtStart, powerAndCostByStageOrdinal) =>
						flowProjectionPieceCalculator.calc(pieceIndex, inputQueueAtStart, closedGraph.fromIterable(powerAndCostByStageOrdinal.map(_.power))) {
							source => upstreamTrajectoryBySource(source).getValueAt(pieceIndex)
						}
				};
				val pieceExpirationCostRef: Ref[ExpirationCost] = graphProjectionRef.map {
					graphProjection =>
						val processed = graphProjection.map(_.processed);
						expirationCostAtCompletionPieceCalculator.calcExpirationCost(piecewiseAlgebra.firstPieceStartingInstant, start, end, processed)
				};
				val accumulatedExpirationCostRef: Ref[Money] = sheet.map2(previousPieceAccumulatedExpirationCostRef, pieceExpirationCostRef) {
					(previousAccumulatedExpirationCost, expirationCost) => previousAccumulatedExpirationCost.plus(expirationCost.total)
				};
				previousPieceAccumulatedExpirationCostRef = accumulatedExpirationCostRef;

				val resultAtPieceRef: Ref[ResultAtPiece] = sheet.map6(piecePowerCostRef, powerAndCostByStageOrdinalRef, accumulatedPowerCostRef, pieceExpirationCostRef, accumulatedExpirationCostRef, graphProjectionRef) {
					(piecePowerCost, powerAndCostByStageOrdinal, accumulatedPowerCost, expirationCost, accumulatedExpirationCost, graphProjection) =>
					ResultAtPiece(piecePowerCost, closedGraph.fromIterable(powerAndCostByStageOrdinal.map(_.cost)), accumulatedPowerCost, expirationCost, accumulatedExpirationCost, graphProjection)
				};
				ArgumentsAndResultAtPiece(powerAndCostRefByStage, resultAtPieceRef)
		} {
			(_, argumentsAndResultAtPiece) => argumentsAndResultAtPiece.resultRef.map(_.graphProjection.map(_.inputQueueAtEnd))
		}
		// Add all the [[Sheet]] cells - END
	}
}
