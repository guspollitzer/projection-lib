package wms.flow.planner
package queue

import scala.collection.immutable.Queue

import global.Quantity

import queue.Heap

type FifoQueue = List[Heap]

given QueueOps[FifoQueue] with {
	extension (queue: FifoQueue) def appended(heap: Heap): FifoQueue = queue.appended(heap)

	extension (queue: FifoQueue) def consumed(quantityToConsume: Quantity): Consumption[FifoQueue] = {
		assert(quantityToConsume >= 0)

		if quantityToConsume == 0 then {
			Consumption(queue, Nil, 0)
		} else queue match {
			case Nil =>
				Consumption(Nil, Nil, quantityToConsume)

			case head::tail =>
				val takenFromHeap = head.take(quantityToConsume)
				if takenFromHeap.excess == 0 then {
					Consumption[FifoQueue](takenFromHeap.remaining::tail, List(takenFromHeap.consumed), 0)
				} else {
					assert(takenFromHeap.consumed.nonEmpty)
					val consumedFromTail = tail.consumed(takenFromHeap.excess)
					Consumption[FifoQueue](consumedFromTail.remaining, takenFromHeap.consumed :: consumedFromTail.consumed, consumedFromTail.excess)
				}
		}
	}
}