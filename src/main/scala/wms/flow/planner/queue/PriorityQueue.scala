package wms.flow.planner
package queue

import global.*
import math.Fractionable
import util.TypeId
import time.*
import graph.Stage

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.TreeMap
import scala.collection.{mutable, MapView}
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*

type PriorityQueue = TreeMap[Priority, Heap]

object PriorityQueue {
	def from(treeMap: TreeMap[Priority, Heap]): PriorityQueue = treeMap
}

given QueueOps[PriorityQueue] with {

	extension (thisQueue: PriorityQueue) {

		override def load: Quantity = thisQueue.view.values.map(heap => heap.total).sum

		override def heapIterator: Iterator[Heap] = thisQueue.iterator.map(_._2);

		override def quantityAtCategoryIterator: Iterator[(Category, Quantity)] = {
			for {
				heap <- thisQueue.valuesIterator
				(category, quantity) <- heap.iterator
			}
			yield category -> quantity
		}

		override def filterByCategory(predicate: Category => Boolean): PriorityQueue = thisQueue.map {
			(priority, heap) => priority -> heap.filteredByCategory(predicate)
		}


		override def appended(heapToAdd: Heap): PriorityQueue = {
			var workingMap = thisQueue
			for (category, quantityToAdd) <- heapToAdd.iterator do {
				val newHeapAtPriority: Heap = workingMap.get(category.priority) match {
					case Some(oldHeapAtPriority) => oldHeapAtPriority.add(category, quantityToAdd)
					case None => Heap(Map(category -> quantityToAdd))
				}
				workingMap = workingMap.updated(category.priority, newHeapAtPriority)
			}
			workingMap
		}

		override def mergedWith(thatQueue: PriorityQueue): PriorityQueue = {
			if thatQueue.isEmpty then thisQueue
			else if thisQueue.isEmpty then {
				thatQueue
			} else {
				val collector = new java.util.HashMap(thisQueue.asJava)
				for (priority, thatHeap) <- thatQueue do collector.merge(priority, thatHeap, (a, b) => a.mixedWith(b))
				TreeMap.from(collector.asScala)
			}
		}

		override def except(thatQueue: PriorityQueue): PriorityQueue = {
			if thisQueue.isEmpty || thatQueue.isEmpty then thisQueue
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

		override def consumed(quantityToConsume: Quantity)(using atStage: Stage, atPiece: PieceIndex): Consumption[PriorityQueue] = {
			@tailrec
			def loop(remaining: PriorityQueue, quantityToConsume: Quantity, alreadyConsumed: PriorityQueue): Consumption[PriorityQueue] = {
				assert(quantityToConsume >= 0)
				if quantityToConsume == 0 then {
					Consumption(remaining, alreadyConsumed, 0)
				} else {
					remaining.headOption match {
						case None =>
							Consumption(TreeMap.empty, alreadyConsumed, quantityToConsume)

						case Some((priority, heap)) =>
							val consumption: Consumption[Heap] = heap.consume(quantityToConsume);
							if (consumption.remaining.nonEmpty) {
								Consumption(remaining.updated(priority, consumption.remaining), alreadyConsumed + (priority -> consumption.consumed), 0)
							} else {
								assert(consumption.consumed == heap)
								loop(remaining.tail, consumption.shortage, alreadyConsumed + (priority -> heap))
							}
					}
				}
			}

			loop(thisQueue, quantityToConsume, TreeMap.empty)
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

given TypeId[PriorityQueue] = new TypeId[PriorityQueue] {}

