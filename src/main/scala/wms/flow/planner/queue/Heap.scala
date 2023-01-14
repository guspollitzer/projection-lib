package wms.flow.planner
package queue

import global.*
import math.Fractionable
import graph.Stage

import scala.jdk.StreamConverters.*
import scala.collection.immutable.{HashMap, Map}
import scala.collection.MapView

object Heap {
	sealed trait Event
	case class Consumed(atStage: Stage, atPiece: PieceIndex) extends Event

}

import Heap.*

class Heap(private[Heap] val quantityByCategory: Map[Category, Quantity], val history: List[Event] = Nil) {

	def isEmpty: Boolean = quantityByCategory.isEmpty
	def nonEmpty: Boolean = quantityByCategory.nonEmpty

	def iterator: Iterator[(Category, Quantity)] = quantityByCategory.iterator

	def total: Quantity = quantityByCategory.iterator.map(_._2).sum

	def filteredByCategory(f: Category => Boolean): Heap = new Heap(quantityByCategory.view.filterKeys(f).toMap)

	def takeFraction(fraction: Quantity): Heap = Heap(calcFraction(fraction))

	private def calcFraction(fraction: Quantity): Map[Category, Quantity] = quantityByCategory.map((c, q) => c -> q * fraction)

	def mixedWith(that: Heap): Heap = {
		Heap(
			HashMap.from(quantityByCategory).merged(HashMap.from(that.quantityByCategory))((thisEntry, thatEntry) => {
				assert(thisEntry._2 > 0 && thatEntry._2 > 0)
				thisEntry._1 -> (thisEntry._2 + thatEntry._2)
			}
			)
		)
	}

	def add(category: Category, quantityToAdd: Quantity): Heap = {
		assert(quantityToAdd > 0)
		val newQbc = quantityByCategory.get(category) match {
			case Some(oldQuantityAtCategory) => quantityByCategory.updated(category, oldQuantityAtCategory + quantityToAdd)
			case None => quantityByCategory + (category -> quantityToAdd)
		}
		Heap(quantityByCategory)
	}

	def consume(quantityToConsume: Quantity)(using atStage: Stage, atPiece: PieceIndex): Consumption[Heap] = {
		val quantityAvailable: Quantity = quantityByCategory.values.sum;
		assert(quantityAvailable >= 0);
		if quantityToConsume >= quantityAvailable then {
			Consumption(Heap(Map.empty), this, quantityToConsume - quantityAvailable)
		} else {
			val consumedFraction = quantityToConsume / quantityAvailable;
			assert(consumedFraction >= 0);
			val remainingFraction = quantityAvailable - consumedFraction;
			val remaining = this.takeFraction(remainingFraction);
			val consumed = Heap(this.calcFraction(consumedFraction), Consumed(atStage, atPiece) :: this.history);
			Consumption(remaining, consumed, 0)
		}
	}

	def without(that: Heap): Heap = {
		if this.quantityByCategory.isEmpty || that.quantityByCategory.isEmpty then this
		else {
			val builder = Map.newBuilder[Category, Quantity]
			for (category, thisQuantity) <- this.quantityByCategory do {
				that.quantityByCategory.get(category) match {
					case None => builder.addOne(category -> thisQuantity)
					case Some(thatQuantity) =>
						val newQuantity = thisQuantity - thatQuantity
						if newQuantity > 0 then builder.addOne(category -> newQuantity)
				}
			}
			Heap(builder.result(), Nil) // TODO: include the history or not?
		}
	}
}

