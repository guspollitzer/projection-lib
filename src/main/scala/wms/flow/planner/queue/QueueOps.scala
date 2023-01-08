package wms.flow.planner
package queue

import global.*
import graph.Stage
import time.*

import scala.collection.MapView

trait QueueOps[Q] {
	extension (queue: Q) {
		def load: Quantity

		def heapIterator: Iterator[Heap]
		/** @deprecated because is not used */
		def quantityAtCategoryIterator: Iterator[(Category, Quantity)]
		def filterByCategory(predicate: Category => Boolean): Q
		/** @deprecated because is not used */
		def appended(heap: Heap): Q
		/** Creates a queue that is the result of merging two queues as if they where generated simultaneously at constant speed.
		  * For example, if the queues were FIFO with loads (a1, a2) and (b1,b2,b3), then the resulting queue would be (b1,a1,b2,b3,a2) or (b1,a1,b2,a2,b3) */
		def mergedWith(thatQueue: Q): Q
		def except(thatQueue: Q): Q
		def consumed(quantity: Quantity)(using atStage: Stage, atPiece: PieceIndex): Consumption[Q]

		/** Travels the [[Heap]]s of this queue in order, giving the time fragments during which each heap would be consumed if the whole queue consumption started and ended at the specified instants; assuming the consumption speed is constant.
		  *
		  * Design note: It was chosen to use a traveler instead of returning a collection, just for efficiency.
		  * @param consumptionStart the hypothetical instant when the consumption of this queue starts.
		  * @param consumptionEnd the hypothetical instant when the consumption of this queue ends.
		  * @param initialState the initial state of the traveler
		  * @param travelerNextStateFunc a function that, given the state of the traveler at the start of a fragment (and some other relevant information like the heap load), calculates the state of the traveler at the end of said fragment.
		  * @return the state of the traveler at the end of the last fragment (the consumption's end). */
		def travelConsumptionFragments[S](consumptionStart: Instant, consumptionEnd: Instant, initialState: S)
			(travelerNextStateFunc: (fragmentStart: Instant, fragmentEnd: Instant, heap: Heap, heapLoad: Quantity, accumulatedLoad: Quantity, state: S) => S)
		: S = {
			val durationPerElement = (consumptionEnd - consumptionStart) / load;
			val iterator = heapIterator;
			var start = consumptionStart;
			var state: S = initialState;
			var accumulatedLoad: Quantity = 0;
			while (iterator.nonEmpty) {
				val heap = iterator.next();
				val heapLoad = heap.total;
				accumulatedLoad += heapLoad;
				val end = consumptionStart + durationPerElement * accumulatedLoad;
				state = travelerNextStateFunc(start, end, heap, heapLoad, accumulatedLoad, state);
				start = end
			}
			assert(scala.math.abs(start - consumptionEnd) < (consumptionEnd - consumptionStart) / 1e6)
			state
		}
	}
}
