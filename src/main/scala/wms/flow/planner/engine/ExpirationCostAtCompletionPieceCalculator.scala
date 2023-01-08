package wms.flow.planner
package engine

import global.*
import graph.*
import queue.{*, given}
import resource.*
import time.*
import util.SmartArray
import workflow.*


object ExpirationCostAtCompletionPieceCalculator {

	trait ExpirationExpectancyFunc {
		/** The implementation should calculate the expected value of the amount of elements that will be completed after the deadline for a set of elements that, according to the projection, are completed (at constant speed) during the specified interval.
		  * The implementation should support, not only the case when the interval ends before the deadline but also when after. Even when the interval starts after the deadline.
		  *
		  * In other words, the implementation should calculate the definite integral, on the specified interval, of the function that, given the expected completion time of an element, calculates the probability that the real completion time is after the deadline.
		  * @param projectionDate the date when the projection calculation is made.
		  * @param projectedStartOfProcessing the date when the processing completion of the set of elements starts, according to the calculated projection.
		  * @param projectedEndOfProcessing the date when the processing completion of the set of elements ends, according to the calculated projection.
		  * @param quantityOfElements the amount of elements in the set.
		  * @param deadline the processing completion deadline.
		  * */
		def apply(projectionDate: Instant, projectedStartOfProcessing: Instant, projectedEndOfProcessing: Instant, quantityOfElements: Quantity, deadline: Instant): Quantity
	}

	/** The implementation should calculate the cost of that an element's processing is completed after its deadline. */
	type ExpirationCostFunc = (channel: Channel, path: Path) => Money
	case class SinkParams(expirationExpectancyFunc: ExpirationExpectancyFunc, expirationCostFunc: ExpirationCostFunc)

	/** The distribution of the total expiration cost among the stages and pieces that were involved. */
	type InvolvementGrid = SmartArray[Array[Money]]
	case class ExpirationCost(total: Money, bySink: Map[SinkN[?], Money], involvedPiecesAndStagesGrid: InvolvementGrid)
}

import ExpirationCostAtCompletionPieceCalculator.*

class ExpirationCostAtCompletionPieceCalculator[CG <: ClosedGraph](val closedGraph: CG)(sinksParams: Map[SinkN[?], SinkParams]) {
	import closedGraph.*

	/** Calculates the expected value of the cost due to expired elements (those whose processing is completed after the deadline), corresponding to the elements for which, according to the calculated projection, its processing will be completed during the specified interval.
	  * The specified interval should be a piece of the [[StaggeredAlgebra]] used to calculate the projection.
	  *
	  * @param projectionDate the instant when the projection was made.
	  * @param pieceStart the lower bound of the time interval.
	  * @param pieceEnd the upper bound of the time interval.
	  * @param amountOfCompletionsBySink the amount of elements that will be completed at each sink-stage during the interval; grouped by category, in completion order.
	  * @return the expected value of the expiration cost corresponding to the elements whose processing is completed during the interval. */
	def calcExpirationCost(
		projectionDate: Instant,
		pieceStart: Instant,
		pieceEnd: Instant,
		amountOfCompletionsBySink: SinkN[?] => Queue,
	): ExpirationCost = {

		var totalCost = ZERO_MONEY;
		val involvedPiecesAndStagesGrid: InvolvementGrid = new SmartArray(new Array[Money](closedGraph.size));

		val costBySink = for (sink, sinkParams) <- sinksParams yield {

			def traveler(fragmentStart: Instant, fragmentEnd: Instant, heap: Heap, heapLoad: Quantity, accumulatedLoad: Quantity, expectedValueOfExpirationCostAccumulator: Money): Money = {
				// calculate the expiration cost associated to the iterated heap
				var expectedValueOfExpirationCostCorrespondingToHeap: Money = ZERO_MONEY;
				for (category, quantity) <- heap.iterator do {
					val expectedValueOfAmountOfExpiredElements: Quantity = sinkParams.expirationExpectancyFunc(projectionDate, fragmentStart, fragmentEnd, quantity, category.priority);
					val singleElementExpirationCost: Money = sinkParams.expirationCostFunc(category.channel, category.path);
					val expectedValueOfExpirationCost: Money = singleElementExpirationCost.multipliedBy(expectedValueOfAmountOfExpiredElements);
					expectedValueOfExpirationCostCorrespondingToHeap = expectedValueOfExpirationCostCorrespondingToHeap.plus(expectedValueOfExpirationCost)
				}

				// update the involvement grid
				val weight = 1d / heap.history.size;
				for Heap.Consumed(atStage, atPiece) <- heap.history do {
					val involvedStagesAtPiece = involvedPiecesAndStagesGrid(atPiece);
					involvedStagesAtPiece(atStage.ordinal) = involvedStagesAtPiece(atStage.ordinal).plus(expectedValueOfExpirationCostCorrespondingToHeap).multipliedBy(weight)
				}

				expectedValueOfExpirationCostAccumulator.plus(expectedValueOfExpirationCostCorrespondingToHeap);
			}

			val expectedValueOfExpirationTotalCostCorrespondingToSink: Money = amountOfCompletionsBySink(sink).travelConsumptionFragments[Money](pieceStart, pieceEnd, ZERO_MONEY)(traveler);
			totalCost = totalCost.plus(expectedValueOfExpirationTotalCostCorrespondingToSink);
			sink -> expectedValueOfExpirationTotalCostCorrespondingToSink
		}

		ExpirationCost(totalCost, costBySink, involvedPiecesAndStagesGrid)
	}


}
