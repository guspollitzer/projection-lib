package wms.flow.planner
package queue

import global.{Category, Priority, Quantity}
import math.Fractionable
import util.TypeId

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*

type PriorityQueue = TreeMap[Priority, Heap]

object PriorityQueue {
	def from(treeMap: TreeMap[Priority, Heap]): PriorityQueue = treeMap
}

given QueueOps[PriorityQueue] with {

	extension (thisQueue: PriorityQueue) {

		def load: Quantity = thisQueue.view.values.map(heap => heap.total).sum

		def appended(heapToAdd: Heap): PriorityQueue = {
			var workingMap = thisQueue
			for (category, quantityToAdd) <- heapToAdd do {
				val newHeapAtPriority: Heap = workingMap.get(category.priority) match {
					case Some(oldHeapAtPriority) => oldHeapAtPriority.add(category, quantityToAdd)
					case None => Map(category -> quantityToAdd)
				}
				workingMap = workingMap.updated(category.priority, newHeapAtPriority)
			}
			workingMap
		}

		def mergedWith(thatQueue: PriorityQueue): PriorityQueue = {
			if thatQueue.isEmpty then thisQueue
			else if thisQueue.isEmpty then thatQueue
			else {
				val collector = new java.util.HashMap(thisQueue.asJava)
				for (priority, thatHeap) <- thatQueue do collector.merge(priority, thatHeap, (a, b) => a.mixedWith(b))
				TreeMap.from(collector.asScala)
			}
		}

		def except(thatQueue: PriorityQueue): PriorityQueue = {
			if thatQueue.isEmpty then thisQueue
			else {
				val builder = TreeMap.newBuilder[Priority, Heap]
				for (priority, thisHeap) <- thisQueue do {
					thatQueue.get(priority) match {
						case None => builder.addOne(priority -> thisHeap)
						case Some(thatHeap) =>
							val newHeap = thisHeap.without(thatHeap)
							if newHeap.nonEmpty then builder.addOne(priority -> newHeap)
					}
				}
				builder.result()
			}
		}

		def consumed(quantityToConsume: Quantity): Consumption[PriorityQueue] = {
			loop(quantityToConsume, TreeMap.empty)
		}

		@tailrec
		private def loop(quantityToConsume: Quantity, alreadyConsumed: PriorityQueue): Consumption[PriorityQueue] = {
			assert(quantityToConsume >= 0)
			if quantityToConsume == 0 then {
				Consumption(thisQueue, alreadyConsumed, 0)
			} else {
				thisQueue.headOption match {
					case None =>
						Consumption(TreeMap.empty, alreadyConsumed, quantityToConsume)

					case Some((priority, heap)) =>
						val x: Consumption[Heap] = heap.consume(quantityToConsume)
						if (x.remaining.nonEmpty) {
							Consumption(thisQueue.updated(priority, x.remaining), alreadyConsumed + (priority -> x.consumed), 0)
						} else {
							assert(x.consumed == heap)
							thisQueue.tail.loop(x.excess, alreadyConsumed + (priority -> heap))
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
	extension (pq: PriorityQueue) def takeFraction(fraction: Quantity): PriorityQueue =
		pq.map((priority, heap) => priority -> heap.takeFraction(fraction))
}

given TypeId[PriorityQueue] = new TypeId[PriorityQueue]{}

