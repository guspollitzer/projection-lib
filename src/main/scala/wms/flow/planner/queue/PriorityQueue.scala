package wms.flow.planner
package queue

import global.{Category, Priority, Quantity}
import math.Fractionable

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.TreeMap

type PriorityQueue = TreeMap[Priority, Heap]

given QueueOps[PriorityQueue] with {
	extension (queue: PriorityQueue) {
		def appended(heapToAdd: Heap): PriorityQueue = {
			var workingMap = queue
			for (category, quantityToAdd) <- heapToAdd do {
				val newHeapAtPriority: Heap = workingMap.get(category.priority) match {
					case Some(oldHeapAtPriority) => oldHeapAtPriority.append(category, quantityToAdd)
					case None => Map(category -> quantityToAdd)
				}
				workingMap = workingMap.updated(category.priority, newHeapAtPriority)
			}
			workingMap
		}

		def consumed(quantityToConsume: Quantity): Consumption[PriorityQueue] = {
			loop(quantityToConsume, TreeMap.empty)
		}

		@tailrec
		private def loop(quantityToConsume: Quantity, alreadyConsumed: PriorityQueue): Consumption[PriorityQueue] = {
			assert(quantityToConsume >= 0)
			if quantityToConsume == 0 then {
				Consumption(queue, alreadyConsumed, 0)
			} else {
				queue.headOption match {
					case None =>
						Consumption(TreeMap.empty, alreadyConsumed, quantityToConsume)

					case Some((priority, heap)) =>
						val x: Consumption[Heap] = heap.consume(quantityToConsume)
						if (x.remaining.nonEmpty) {
							Consumption(queue.updated(priority, x.remaining), alreadyConsumed + (priority -> x.consumed), 0)
						} else {
							assert(x.consumed == heap)
							queue.tail.loop(x.excess, alreadyConsumed + (priority -> heap))
						}
				}
			}
		}
	}
}

given EmptyAble[PriorityQueue] with {
	extension (c: PriorityQueue) def isEmpty = c.isEmpty
}

given Concatenable[PriorityQueue] with {
	def empty: PriorityQueue = TreeMap.empty

	extension (a: PriorityQueue) @targetName("concat") def ++(b: PriorityQueue) = a ++ b
}

given Fractionable[PriorityQueue] with {
	extension (pq: PriorityQueue) def takeFraction(fraction: Float): PriorityQueue =
		pq.map((priority, heap) => priority -> heap.takeFraction(fraction))
}