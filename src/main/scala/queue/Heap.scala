package wms.flow.planner
package queue

import global.{Quantity, Category}

type Heap = Map[Category, Quantity]

extension (heap: Heap) {
	def take(quantityToConsume: Quantity): Consumption[Heap] = {
		val heapTotal: Quantity = heap.values.sum
		if quantityToConsume >= heapTotal then {
			Consumption(Map.empty, heap, quantityToConsume - heapTotal)
		} else {
			val consumedFraction = quantityToConsume / heapTotal
			val remainingFraction = heapTotal - consumedFraction;
			val remaining = heap.map((c, q) => c -> q * remainingFraction)
			val consumed = heap.map((c, q) => c -> q * remainingFraction)
			Consumption(remaining, consumed, 0)
		}
	}
}