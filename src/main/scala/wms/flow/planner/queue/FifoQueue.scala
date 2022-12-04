package wms.flow.planner
package queue

import global.Quantity
import queue.{total, Heap}
import util.TypeId
import wms.flow.planner.math.Fractionable

import scala.annotation.{tailrec, targetName}

type FifoQueue = List[Heap]

given QueueOps[FifoQueue] with {
	extension (queue: FifoQueue) {

		def load: Quantity = queue.view.map[Quantity](h => h.total).sum

		def appended(heap: Heap): FifoQueue = queue.appended(heap)

		def consumed(quantityToConsume: Quantity): Consumption[FifoQueue] = {
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
						if takenFromHead.excess == 0 then {
							Consumption[FifoQueue](
								takenFromHead.remaining :: tail,
								(takenFromHead.consumed :: alreadyConsumed).reverse,
								0
							)
						} else {
							assert(takenFromHead.consumed.nonEmpty)
							tail.loop(takenFromHead.excess, takenFromHead.consumed :: alreadyConsumed)
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