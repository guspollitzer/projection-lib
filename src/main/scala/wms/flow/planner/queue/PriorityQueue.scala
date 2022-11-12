package wms.flow.planner
package queue

import global.{Category, Priority, Quantity}

import scala.collection.immutable.TreeMap

type PriorityQueue = TreeMap[Priority, Heap]

given QueueOps[PriorityQueue] with {
	extension (queue: PriorityQueue) {
		def appended(heap: Heap): PriorityQueue =
			queue.concat(heap.groupBy(h => h._1.priority))

		def consumed(quantityToConsume: Quantity): Consumption[PriorityQueue] = {
			loop(quantityToConsume, TreeMap.empty)
		}

		private def loop(quantityToConsume: Quantity, alreadyConsumed: PriorityQueue): Consumption[PriorityQueue] = {
			assert(quantityToConsume >= 0)
			if quantityToConsume == 0 then {
				Consumption(queue, TreeMap.empty, 0)
			} else {
				queue.headOption match {
					case None =>
						Consumption(TreeMap.empty, TreeMap.empty, quantityToConsume)

					case Some((priority, heap)) =>
						val x: Consumption[Heap] = heap.consume(quantityToConsume)
						if (x.remaining.nonEmpty) {
							Consumption(queue.updated(priority, x.remaining), TreeMap(priority -> x.consumed), 0)
						} else {
							queue.tail.loop(x.excess, alreadyConsumed + (priority -> x.consumed))
						}
				}
			}
		}
	}
}

given EmptyAble[PriorityQueue] with {
	extension (c: PriorityQueue) def isEmpty = c.isEmpty
}
