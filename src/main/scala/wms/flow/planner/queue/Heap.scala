package wms.flow.planner
package queue

import global.{Quantity, Category}

type Heap = Map[Category, Quantity]

extension (heap: Heap) {
	def consume(quantityToConsume: Quantity): Consumption[Heap] = {
		val quantityAvailable: Quantity = heap.values.sum
		if quantityToConsume >= quantityAvailable then {
			Consumption(Map.empty, heap, quantityToConsume - quantityAvailable)
		} else {
			val consumedFraction = quantityToConsume / quantityAvailable
			val remainingFraction = quantityAvailable - consumedFraction
			val remaining = heap.map((c, q) => c -> q * remainingFraction)
			val consumed = heap.map((c, q) => c -> q * remainingFraction)
			Consumption(remaining, consumed, 0)
		}
	}
}

given EmptyAble[Heap] with {
	extension (heap: Heap) def isEmpty = heap.isEmpty

}

