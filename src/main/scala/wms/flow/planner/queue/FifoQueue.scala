package wms.flow.planner
package queue

import global.{Category, Quantity}
import math.Fractionable
import queue.{total, Heap}
import time.*
import util.TypeId

import scala.annotation.{tailrec, targetName}

type FifoQueue = List[Heap]

given QueueOps[FifoQueue] with {
	extension (thisQueue: FifoQueue) {

		override def load: Quantity = thisQueue.view.map[Quantity](h => h.total).sum

		override def heapIterator: Iterator[Heap] = thisQueue.iterator;

		override def quantityAtCategoryIterator: Iterator[(Category, Quantity)] = {
			for {
				heap <- thisQueue.iterator
				(category, quantity) <- heap.iterator
			}
			yield category -> quantity
		}

		override def filterByCategory(predicate: Category => Boolean): FifoQueue = thisQueue.map(heap => heap.filteredByCategory(predicate))

		override def appended(heap: Heap): FifoQueue = thisQueue.appended(heap)

		override def mergedWith(thatQueue: FifoQueue): FifoQueue = {
			// Not implemented because perhaps the case of two FIFO queues joining does not exist in Flow
			// Because here we lack all the necessary information to know which element was  simplicity the implementation may assume that the two queue where generated simultaneously and at constant speed
			???
		}

		override def except(thatQueue: FifoQueue): FifoQueue = thisQueue

		override def consumed(quantityToConsume: Quantity): Consumption[FifoQueue] = {
			assert(quantityToConsume >= 0)
			loop(quantityToConsume, Nil)
		}

		@tailrec
		private def loop(quantityToConsume: Quantity, alreadyConsumed: List[Heap]): Consumption[FifoQueue] = {
			if quantityToConsume == 0 then {
				Consumption(thisQueue, alreadyConsumed.reverse, 0)
			} else {
				thisQueue match {
					case Nil => Consumption(Nil, alreadyConsumed.reverse, quantityToConsume)

					case head :: tail =>
						val takenFromHead = head.consume(quantityToConsume)
						if takenFromHead.shortage == 0 then {
							Consumption[FifoQueue](
								takenFromHead.remaining :: tail,
								(takenFromHead.consumed :: alreadyConsumed).reverse,
								0
							)
						} else {
							assert(takenFromHead.consumed.nonEmpty)
							tail.loop(takenFromHead.shortage, takenFromHead.consumed :: alreadyConsumed)
						}
				}
			}
		}
	}
}


given EmptyAble[FifoQueue] with {
	extension (c: FifoQueue) def isEmpty = c.isEmpty
}

given Concatenable[FifoQueue] with {
	override def empty: FifoQueue = Nil

	extension (a: FifoQueue) @targetName("concat") def ++(b: FifoQueue) = a ++ b
}

given Fractionable[FifoQueue] with {
	extension (fq: FifoQueue) def takeFraction(fraction: Quantity): FifoQueue =
		fq.map(_.takeFraction(fraction))
}

given TypeId[FifoQueue] = new TypeId[FifoQueue]{}