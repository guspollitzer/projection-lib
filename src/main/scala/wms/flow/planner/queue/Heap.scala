package wms.flow.planner
package queue

import global.{Category, Quantity}
import wms.flow.planner.math.Fractionable

type Heap = Map[Category, Quantity]

extension (heap: Heap) {

	def append(category: Category, quantityToAdd: Quantity): Heap =
		heap.get(category) match {
			case Some(oldQuantityAtCategory) => heap.updated(
				category,
				oldQuantityAtCategory + quantityToAdd
			)
			case None => heap + (category -> quantityToAdd)
		}
		
	def consume(quantityToConsume: Quantity): Consumption[Heap] = {
		val quantityAvailable: Quantity = heap.values.sum
		if quantityToConsume >= quantityAvailable then {
			Consumption(Map.empty, heap, quantityToConsume - quantityAvailable)
		} else {
			val consumedFraction = quantityToConsume / quantityAvailable
			val remainingFraction = quantityAvailable - consumedFraction
			val remaining = heap.takeFraction(remainingFraction)
			val consumed = heap.takeFraction(consumedFraction)
			Consumption(remaining, consumed, 0)
		}
	}
}

given EmptyAble[Heap] with {
	extension (heap: Heap) def isEmpty = heap.isEmpty

}

given Fractionable[Heap] with {
	extension (heap: Heap) def takeFraction(fraction: Float): Heap = heap.map((c, q) => c -> q * fraction)
}

