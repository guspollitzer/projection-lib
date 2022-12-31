package wms.flow.planner
package queue

import global.*
import math.Fractionable
import time.*
import util.*

import scala.annotation.targetName

type Queue = OneOf[PriorityQueue, FifoQueue]

given QueueOps[Queue] with {
	extension (thisQueue: Queue) {
		override def load: Quantity = thisQueue match {
			case CaseA(priorityQueue) => priorityQueue.load;
			case CaseB(fifoQueue) => fifoQueue.load;
		}

		override def heapIterator: Iterator[Heap] = thisQueue match {
			case CaseA(priorityQueue) => priorityQueue.heapIterator
			case CaseB(fifoQueue) => fifoQueue.heapIterator
		}

		override def quantityAtCategoryIterator: Iterator[(Category, Quantity)] = thisQueue match {
			case CaseA(priorityQueue) => priorityQueue.quantityAtCategoryIterator
			case CaseB(fifoQueue) => fifoQueue.quantityAtCategoryIterator
		}
		override def appended(heapToAdd: Heap): Queue = thisQueue match {
			case CaseA(priorityQueue) => CaseA(priorityQueue.appended(heapToAdd))
			case CaseB(fifoQueue) => CaseB(fifoQueue.appended(heapToAdd))
		}

		override def mergedWith(thatQueue: Queue): Queue = {
			(thisQueue, thatQueue) match {
				case (CaseA(thisPriorityQueue), CaseA(thatPriorityQueue)) => CaseA(thisPriorityQueue.mergedWith(thatPriorityQueue))
				case (CaseA(thisPriorityQueue), CaseB(thatFifoQueue)) => ???
				case (CaseB(thisFifoQueue), CaseA(thatPriorityQueue)) => ???
				case (CaseB(thisFifoQueue), CaseB(thatFifoQueue)) => CaseB(thisFifoQueue.mergedWith(thatFifoQueue))
			}
		}

		override def except(thatQueue: Queue): Queue = {
			(thisQueue, thatQueue) match {
				case (CaseA(thisPriorityQueue), CaseA(thatPriorityQueue)) => CaseA(thisPriorityQueue.except(thatPriorityQueue))
				case (CaseA(thisPriorityQueue), CaseB(thatFifoQueue)) => ???
				case (CaseB(thisFifoQueue), CaseA(thatPriorityQueue)) => ???
				case (CaseB(thisFifoQueue), CaseB(thatFifoQueue)) => CaseB(thisFifoQueue.except(thatFifoQueue))
			}
		}

		override def consumed(quantityToConsume: Quantity): Consumption[Queue] = thisQueue match {
			case CaseA(priorityQueue) => priorityQueue.consumed(quantityToConsume).map(CaseA(_));
			case CaseB(fifoQueue) => fifoQueue.consumed(quantityToConsume).map(CaseB(_));
		}

		override def filterByCategory(predicate: Category => Boolean): Queue = thisQueue match {
			case CaseA(priorityQueue) => CaseA(priorityQueue.filterByCategory(predicate))
			case CaseB(fifoQueue) => CaseB(fifoQueue.filterByCategory(predicate))
		}
	}
}


given EmptyAble[Queue] with {
	extension (c: Queue) def isEmpty = c match {
		case CaseA(priorityQueue) => priorityQueue.isEmpty
		case CaseB(fifoQueue) => fifoQueue.isEmpty
	}
}

given Concatenable[Queue] with {
	def empty: Queue = CaseB(Nil)

	extension (a: Queue) {
		@targetName("concat")
		def ++(b: Queue) = (a, b) match {
			case (CaseA(pqA), CaseA(pqB)) => CaseA(pqA ++ pqB)
			case (CaseB(fqA), CaseB(fqB)) => CaseB(fqA ++ fqB)
			case _ => ???
		}
	}
}

given Fractionable[Queue] with {
	extension (queue: Queue) def takeFraction(fraction: Quantity): Queue = queue match {
		case CaseA(pq) => CaseA(pq.takeFraction(fraction))
		case CaseB(fq) => CaseB(fq.takeFraction(fraction))
	}
}

given TypeId[Queue] = new TypeId[Queue] {}
