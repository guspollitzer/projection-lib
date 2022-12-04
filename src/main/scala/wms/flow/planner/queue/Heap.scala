package wms.flow.planner
package queue

import global.{Category, Quantity}
import math.Fractionable

import scala.jdk.StreamConverters._
import scala.collection.immutable.{Map, HashMap}

type Heap = Map[Category, Quantity]

extension (thisHeap: Heap) {

	def total: Quantity = thisHeap.view.values.sum

	def mixedWith(thatHeap: Heap): Heap = {
		HashMap.from(thisHeap).merged(HashMap.from(thatHeap))((thisEntry, thatEntry) => {
			assert(thisEntry._2 > 0 && thatEntry._2 > 0)
			thisEntry._1 -> (thisEntry._2 + thatEntry._2)
		})
	}

	def add(category: Category, quantityToAdd: Quantity): Heap = {
		assert(quantityToAdd > 0)
		thisHeap.get(category) match {
			case Some(oldQuantityAtCategory) => thisHeap.updated(category, oldQuantityAtCategory + quantityToAdd)
			case None => thisHeap + (category -> quantityToAdd)
		}
	}

	def consume(quantityToConsume: Quantity): Consumption[Heap] = {
		val quantityAvailable: Quantity = thisHeap.values.sum
		assert(quantityAvailable >= 0)
		if quantityToConsume >= quantityAvailable then {
			Consumption(Map.empty, thisHeap, quantityToConsume - quantityAvailable)
		} else {
			val consumedFraction = quantityToConsume / quantityAvailable
			assert(consumedFraction >= 0)
			val remainingFraction = quantityAvailable - consumedFraction
			val remaining = thisHeap.takeFraction(remainingFraction)
			val consumed = thisHeap.takeFraction(consumedFraction)
			Consumption(remaining, consumed, 0)
		}
	}

	def without(thatHeap: Heap): Heap = {
		if thatHeap.isEmpty then thisHeap
		else if thisHeap.isEmpty then Map.empty
		else {
			val builder = Map.newBuilder[Category, Quantity]
			for (category, thisQuantity) <- thisHeap do {
				thatHeap.get(category) match {
					case None => builder.addOne(category -> thisQuantity)
					case Some(thatQuantity) =>
						val newQuantity = thisQuantity - thatQuantity
						if newQuantity > 0 then builder.addOne(category -> newQuantity)
				}
			}
			builder.result()
		}
	}

}

given EmptyAble[Heap] with {
	extension (heap: Heap) def isEmpty = heap.isEmpty

}

given Fractionable[Heap] with {
	extension (heap: Heap) def takeFraction(fraction: Quantity): Heap = heap.map((c, q) => c -> q * fraction)
}

