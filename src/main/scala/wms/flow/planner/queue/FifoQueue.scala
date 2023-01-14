package wms.flow.planner
package queue

import global.*
import graph.Stage
import math.Fractionable
import queue.{*, given}
import time.*
import util.TypeId

import scala.annotation.{tailrec, targetName}

type FifoQueue = List[Heap]

given QueueOps[FifoQueue] with {
	extension (thisQueue: FifoQueue) {

		override def load: Quantity = thisQueue.view.map[Quantity](h => h.total).sum

		override def heapIterator: Iterator[Heap] = thisQueue.iterator;

		override def quantityAtCategoryIterator: Iterator[(Category, Quantity)] = {
			for {
				heap <- thisQueue.iterator
				(category, quantity) <- heap.iterator
			}
			yield category -> quantity
		}

		override def filterByCategory(predicate: Category => Boolean): FifoQueue = thisQueue.map(heap => heap.filteredByCategory(predicate))

		override def appended(heap: Heap): FifoQueue = thisQueue.appended(heap)

		override def mergedWith(thatQueue: FifoQueue): FifoQueue = {
			// Not implemented because perhaps the case of two FIFO queues joining does not exist in Flow
			// Because here we don't know which heap was generated first, the implementation may assume that the two queue where generated simultaneously and at constant speed.
			???
		}

		override def except(thatQueue: FifoQueue): FifoQueue = {
			if thisQueue.isEmpty || thatQueue.isEmpty then thisQueue
			else {

				@tailrec
				def loopThis(unprocessedThis: FifoQueue, remainingThat: FifoQueue, alreadyProcessedThis: FifoQueue): FifoQueue = {
					unprocessedThis match {
						case Nil => alreadyProcessedThis
						case headThis :: tailThis =>

							@tailrec
							def loopThat(unprocessedThat: FifoQueue, updatedHeadThis: Heap, alreadyProcessedThat: FifoQueue): (Heap, FifoQueue) = {
								unprocessedThat match {
									case Nil => (updatedHeadThis, alreadyProcessedThat)
									case headThat :: tailThat =>
										loopThat(
											tailThat,
											updatedHeadThis.without(headThat),
											headThat.without(updatedHeadThis) :: alreadyProcessedThat
										)
								}
							}

							val (updatedHeapThis, newRemainingThat) = loopThat(remainingThat, headThis, Nil)
							loopThis(tailThis, newRemainingThat, updatedHeapThis :: alreadyProcessedThis)
					}
				}

				loopThis(thisQueue, thatQueue, Nil).reverse
			}
		}

		override def consumed(quantityToConsume: Quantity)(using atStage: Stage, atPiece: PieceIndex): Consumption[FifoQueue] = {
			assert(quantityToConsume >= 0)

			@tailrec
			def loop(remaining: FifoQueue, quantityToConsume: Quantity, alreadyConsumed: List[Heap]): Consumption[FifoQueue] = {
				if quantityToConsume == 0 then {
					Consumption(remaining, alreadyConsumed.reverse, 0)
				} else {
					remaining match {
						case Nil => Consumption(Nil, alreadyConsumed.reverse, quantityToConsume)

						case head :: tail =>
							val takenFromHead = head.consume(quantityToConsume)
							if takenFromHead.shortage == 0 then {
								Consumption[FifoQueue](
									takenFromHead.remaining :: tail,
									(takenFromHead.consumed :: alreadyConsumed).reverse,
									0
								)
							} else {
								assert(takenFromHead.consumed.nonEmpty)
								loop(remaining.tail, takenFromHead.shortage, takenFromHead.consumed :: alreadyConsumed)
							}
					}
				}
			}

			loop(thisQueue, quantityToConsume, Nil)
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

given TypeId[FifoQueue] = new TypeId[FifoQueue] {}