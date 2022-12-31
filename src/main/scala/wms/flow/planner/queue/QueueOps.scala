package wms.flow.planner
package queue

import scala.collection.MapView

import global.{Quantity, Category}
import time.*

trait QueueOps[Q] {
	extension (queue: Q) {
		def load: Quantity

		def heapIterator: Iterator[Heap]
		def quantityAtCategoryIterator: Iterator[(Category, Quantity)]

		def filterByCategory(predicate: Category => Boolean): Q
		def appended(heap: Heap): Q
		def mergedWith(thatQueue: Q): Q
		def except(thatQueue: Q): Q
		def consumed(quantity: Quantity): Consumption[Q]
		def walkConsumptionTimeDistribution[S](consumptionStart: Instant, consumptionEnd: Instant, initialState: S)
			(walker: (fragmentStart: Instant, fragmentEnd: Instant, heap: Heap, heapLoad: Quantity, accumulatedLoad: Quantity, state: S) => S)
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
				state = walker(start, end, heap, heapLoad, accumulatedLoad, state);
				start = end
			}
			assert(scala.math.abs(start - consumptionEnd) < (consumptionEnd - consumptionStart) / 1e6)
			state
		}
	}
}
