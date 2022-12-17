package wms.flow.planner
package queue

import util.*
import global.*
import math.Fractionable
import scala.annotation.targetName


type Queue = OneOf[PriorityQueue, FifoQueue]

given QueueOps[Queue] with {
	extension (thisQueue: Queue) {
		def load: Quantity = thisQueue match {
			case CaseA(priorityQueue) => priorityQueue.load;
			case CaseB(fifoQueue) => fifoQueue.load;
		}

		def appended(heapToAdd: Heap): Queue = thisQueue match {
			case CaseA(priorityQueue) => CaseA(priorityQueue.appended(heapToAdd))
			case CaseB(fifoQueue) => CaseB(fifoQueue.appended(heapToAdd))
		}

		def mergedWith(thatQueue: Queue): Queue = {
			(thisQueue, thatQueue) match {
				case (CaseA(thisPriorityQueue), CaseA(thatPriorityQueue)) => CaseA(thisPriorityQueue.mergedWith(thatPriorityQueue))
				case (CaseA(thisPriorityQueue), CaseB(thatFifoQueue)) => ???
				case (CaseB(thisFifoQueue), CaseA(thatPriorityQueue)) => ???
				case (CaseB(thisFifoQueue), CaseB(thatFifoQueue)) => CaseB(thisFifoQueue.mergedWith(thatFifoQueue))
			}
		}

		def except(thatQueue: Queue): Queue = {
			(thisQueue, thatQueue) match {
				case (CaseA(thisPriorityQueue), CaseA(thatPriorityQueue)) => CaseA(thisPriorityQueue.except(thatPriorityQueue))
				case (CaseA(thisPriorityQueue), CaseB(thatFifoQueue)) => ???
				case (CaseB(thisFifoQueue), CaseA(thatPriorityQueue)) => ???
				case (CaseB(thisFifoQueue), CaseB(thatFifoQueue)) => CaseB(thisFifoQueue.except(thatFifoQueue))
			}
		}

		def consumed(quantityToConsume: Quantity): Consumption[Queue] = thisQueue match {
			case CaseA(priorityQueue) => priorityQueue.consumed(quantityToConsume).map(CaseA(_));
			case CaseB(fifoQueue) => fifoQueue.consumed(quantityToConsume).map(CaseB(_));
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
