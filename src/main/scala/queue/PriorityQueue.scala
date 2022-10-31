package wms.flow.planner
package queue;

import collection.immutable.TreeMap

import global.{Quantity, Category}

type PriorityQueue = TreeMap[Category, Quantity]

given QueueOps[PriorityQueue] with {
	extension (queue: PriorityQueue) def appended(heap: Heap): PriorityQueue =
		queue.concat(heap)

	extension (queue: PriorityQueue) def consumed(quantityToConsume: Quantity): Consumption[PriorityQueue] = {
		assert(quantityToConsume >= 0)
		if quantityToConsume == 0 then {
			Consumption(queue, TreeMap.empty, 0)
		} else {
			queue.headOption match {
				case None =>
					Consumption(TreeMap.empty, TreeMap.empty, quantityToConsume)

				case Some((category, quantity)) =>
					if quantity > quantityToConsume then {
						val remaining = queue.updated(category, quantity - quantityToConsume)
						val consumed = TreeMap(category -> quantityToConsume)
						Consumption(remaining, queue, 0)
					} else {
						val consumption = queue.tail.consumed(quantityToConsume - quantity)
						Consumption(consumption.remaining, consumption.consumed + (category -> quantity), 0)
					}
			}
		}
	}
}

