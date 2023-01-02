package wms.flow.planner
package engine

import global.*
import graph.*
import queue.{*, given}
import resource.*
import time.*
import workflow.*


object PieceCostCalculator {
	case class StageInfo(
		backlogAtPieceStart: Queue,
		processedDuringPiece: Queue,
		backlogAtPieceEnd: Queue,
		backlogShortageDuringPiece: Quantity,
		desiredBacklogAtEndingInstant: DesiredBacklog
	)

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
	type ExpirationCostFunc = (channel: Channel) => Money
	case class SinkParams(expirationExpectancyFunc: ExpirationExpectancyFunc, expirationCostFunc: ExpirationCostFunc)
}

import PieceCostCalculator.*

class PieceCostCalculator[CG <: ClosedGraph](val closedGraph: CG)(sinksParams: Map[SinkN[?], SinkParams]) {
	import closedGraph.*

	def calcBacklogCost(
		trajectoryStart: Instant,
		pieceStart: Instant,
		pieceEnd: Instant,
		graphInfo: Mapping[StageInfo],
	): Money = {

		var expectedValueOfExpirationTotalCost: Money = ZERO_MONEY;
		for (sink, sinkParams) <- sinksParams do {

			def traveler(fragmentStart: Instant, fragmentEnd: Instant, heap: Heap, heapLoad: Quantity, accumulatedLoad: Quantity, expectedValueOfExpirationCostAccumulator: Money): Money = {
				var expectedValueOfExpirationCostCorrespondingToHeap: Money = ZERO_MONEY;
				for (category, quantity) <- heap do {
					val expectedValueOfAmountOfExpiredElements: Quantity = sinkParams.expirationExpectancyFunc(trajectoryStart, fragmentStart, fragmentEnd, quantity, category.priority);
					val singleElementExpirationCost: Money = sinkParams.expirationCostFunc(category.channel);
					val expectedValueOfExpirationCost: Money = singleElementExpirationCost.multipliedBy(expectedValueOfAmountOfExpiredElements);
					expectedValueOfExpirationCostCorrespondingToHeap = expectedValueOfExpirationCostCorrespondingToHeap.plus(expectedValueOfExpirationCost)
				}
				expectedValueOfExpirationCostAccumulator.plus(expectedValueOfExpirationCostCorrespondingToHeap)
			}

			val expectedValueOfExpirationTotalCostCorrespondingToSink =
				graphInfo.get(sink).processedDuringPiece.travelConsumptionTimeDistribution[Money](pieceStart, pieceEnd, ZERO_MONEY)(traveler)
			expectedValueOfExpirationTotalCost = expectedValueOfExpirationTotalCost.plus(expectedValueOfExpirationTotalCostCorrespondingToSink);
		}

		expectedValueOfExpirationTotalCost
	}



}
