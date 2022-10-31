package wms.flow.planner
package queue

import global.{Quantity, Category}

type Heap = Map[Category, Quantity]

extension (a: Heap) {
	def take(quantityToConsume: Quantity): Consumption[Heap] = {
		val total: Quantity = a.values.sum
		if quantityToConsume >= total then {
			Consumption(Map.empty, a, quantityToConsume - total)
		} else {
			val consumedFraction = quantityToConsume / total
			val remainingFraction = total - consumedFraction;
			val remaining = a.map((c, q) => c -> q * remainingFraction)
			val consumed = a.map((c, q) => c -> q * remainingFraction)
			Consumption(remaining, consumed, 0)
		}
	}
}