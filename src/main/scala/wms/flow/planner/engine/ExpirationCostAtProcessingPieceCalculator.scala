package wms.flow.planner
package engine

import global.*
import graph.*
import queue.{*, given}
import resource.*
import graph.*
import time.*
import workflow.*

object ExpirationCostAtProcessingPieceCalculator {
	class StageInfo(processed: Queue, inputQueueAtEnd: Queue, shortage: Quantity)
}

import ExpirationCostAtProcessingPieceCalculator.*

/** @deprecated it is not implemented because it is easier to calculate it at completion piece. See [[ExpirationCostAtCompletionPieceCalculator]]."  */
class ExpirationCostAtProcessingPieceCalculator[CG <: ClosedGraph](val closedGraph: CG)(sinksParams: Map[SinkN[?], StageInfo]) {
	import closedGraph.*

	/** Calculates the expected value of the cost (transferred in time from the consumption to the processing piece), due to expired elements (those whose processing is completed after the deadline), corresponding to the elements that will be processed (according to a projection) by each stage during the specified interval..
	  * The specified interval should be a piece of the [[StaggeredAlgebra]] used to calculate the projection.
	  *
	  * @param projectionDate the instant when the projection was made.
	  * @param pieceStart the lower bound of the time interval.
	  * @param pieceEnd the upper bound of the time interval.
	  * @param graphInfo the information needed by this method about each stage during the interval, according to a calculated projection.
	  * @return the expected value of the expiration cost corresponding to the elements that are processed by each stage during the interval. */
	def calcExpirationCost(
		projectionDate: Instant,
		pieceStart: Instant,
		pieceEnd: Instant,
		graphInfo: Mapping[StageInfo],
	): Map[SinkN[?], Money] = {

		???
	}
}
