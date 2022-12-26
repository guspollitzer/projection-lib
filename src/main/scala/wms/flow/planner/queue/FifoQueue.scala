package wms.flow.planner
package queue

import global.{Quantity, Category}
import queue.{total, Heap}
import util.TypeId
import wms.flow.planner.math.Fractionable

import scala.annotation.{tailrec, targetName}

type FifoQueue = List[Heap]

given QueueOps[FifoQueue] with {
	extension (queue: FifoQueue) {

		override def load: Quantity = queue.view.map[Quantity](h => h.total).sum

		override def filterByCategory(predicate: Category => Boolean): FifoQueue = queue.map(heap => heap.filteredByCategory(predicate))

		override def appended(heap: Heap): FifoQueue = queue.appended(heap)

		override def mergedWith(thatQueue: FifoQueue): FifoQueue = queue ++ thatQueue

		override def except(thatQueue: FifoQueue): FifoQueue	= queue
		
		override def consumed(quantityToConsume: Quantity): Consumption[FifoQueue] = {
			assert(quantityToConsume >= 0)
			loop(quantityToConsume, Nil)
		}

		@tailrec
		private def loop(quantityToConsume: Quantity, alreadyConsumed: List[Heap]): Consumption[FifoQueue] = {
			if quantityToConsume == 0 then {
				Consumption(queue, alreadyConsumed.reverse, 0)
			} else {
				queue match {
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