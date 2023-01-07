package wms.flow.planner
package engine

import queue.{*, given}
import graph.*
import global.*
import math.*
import util.*

import scala.annotation.targetName

object TaggedQueue {
	sealed trait Tag

	case class Consumed(atStage: Stage, atPiece: PieceIndex) extends Tag

	case class Merged(atStage: Stage, atPiece: PieceIndex, otherHistory: List[Tag]) extends Tag

	case class Concatenated(otherHistory: List[Tag]) extends Tag

	case class FilteredByCategory(atStage: Stage, atPiece: PieceIndex) extends Tag

	case class Fractionated(fraction: Quantity) extends Tag
}

import TaggedQueue.*

/** @deprecated Because the history will grow indefinitely */
case class TaggedQueue(queue: Queue, history: List[Tag]) {
	def withTag(tag: Tag): TaggedQueue = TaggedQueue(queue, tag :: history)

	def load: Quantity = queue.load

	def heapIterator: Iterator[Heap] = queue.heapIterator

	def quantityAtCategoryIterator: Iterator[(Category, Quantity)] = queue.quantityAtCategoryIterator

	def mergedWith(thatQueue: TaggedQueue)(using atStage: Stage, atPiece: PieceIndex): TaggedQueue = {
		val mergedQueue: Queue = this.queue.mergedWith(thatQueue.queue);
		TaggedQueue(mergedQueue, Merged(atStage, atPiece, thatQueue.history) :: history)
	}

	def consumed(quantityToConsume: Quantity)(using atStage: Stage, atPiece: PieceIndex): Consumption[TaggedQueue] = {
		val consumption: Consumption[Queue] = queue.consumed(quantityToConsume);
		val remaining = TaggedQueue(consumption.remaining, history);
		val consumed = TaggedQueue(consumption.consumed, Consumed(atStage, atPiece) :: history);
		Consumption(remaining, consumed, consumption.shortage);
	}

	def filterByCategory(predicate: Category => Boolean)(using atStage: Stage, atPiece: PieceIndex): TaggedQueue = {
		val filteredQueue: Queue = this.queue.filterByCategory(predicate);
		TaggedQueue(filteredQueue, FilteredByCategory(atStage, atPiece) :: history)
	}
}


given EmptyAble[TaggedQueue] with {
	extension (c: TaggedQueue) def isEmpty = c.queue.isEmpty
}

given Concatenable[TaggedQueue] with {
	def empty: TaggedQueue = {
		TaggedQueue(summon[Concatenable[Queue]].empty, Nil)
	}

	extension (a: TaggedQueue) {
		@targetName("concat")
		def ++(b: TaggedQueue): TaggedQueue = {
			given concatOps: Concatenable[Queue] = summon[Concatenable[Queue]]
			TaggedQueue(a.queue ++ b.queue, Concatenated(b.history) :: a.history)
		}
	}
}

given Fractionable[TaggedQueue] with {
	extension (tq: TaggedQueue) def takeFraction(fraction: Quantity): TaggedQueue = {
//		given fractionable: Fractionable[Queue]
		TaggedQueue(tq.queue.takeFraction(fraction), Fractionated(fraction) :: tq.history)
	}
}

given TypeId[TaggedQueue] = new TypeId[TaggedQueue] {}
