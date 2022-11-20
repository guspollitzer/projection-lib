package wms.flow.planner
package queue

import global.Quantity
import queue.Heap
import wms.flow.planner.math.Fractionable

import scala.annotation.targetName
import scala.collection.immutable.Queue

type FifoQueue = List[Heap]

given QueueOps[FifoQueue] with {
	extension (queue: FifoQueue) {
		def appended(heap: Heap): FifoQueue = queue.appended(heap)

		def consumed(quantityToConsume: Quantity): Consumption[FifoQueue] = {
			assert(quantityToConsume >= 0)

			if quantityToConsume == 0 then {
				Consumption(queue, Nil, 0)
			} else {
				queue match {
					case Nil =>
						Consumption(Nil, Nil, quantityToConsume)

					case head :: tail =>
						val takenFromHeap = head.consume(quantityToConsume)
						if takenFromHeap.excess == 0 then {
							Consumption[FifoQueue](takenFromHeap.remaining :: tail, List(takenFromHeap.consumed), 0)
						} else {
							assert(takenFromHeap.consumed.nonEmpty)
							val consumedFromTail = tail.consumed(takenFromHeap.excess)
							Consumption[FifoQueue](consumedFromTail.remaining, takenFromHeap.consumed :: consumedFromTail.consumed, consumedFromTail.excess)
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
	extension (fq: FifoQueue) def takeFraction(fraction: Float): FifoQueue =
		fq.map(_.takeFraction(fraction))
}

