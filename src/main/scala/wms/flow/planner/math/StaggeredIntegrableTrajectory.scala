package wms.flow.planner
package math

import time.*
import PiecewiseIntegrableTrajectory.Piece
import StaggeredIntegrableTrajectory.Step
import queue.Concatenable

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

object StaggeredIntegrableTrajectory {

	case class Step[A: Fractionable](start: Instant, end: Instant, wholeIntegral: A) extends Piece[A] {

		override def integral(from: Instant, to: Instant): A =
			assert(from >= start && to <= end)

			wholeIntegral.takeFraction((to - from) / (end - start))
	}

}

case class StaggeredIntegrableTrajectory[A: Fractionable](byPieceIndex: IndexedSeq[Step[A]])
	(using concatenableOpsForA: Concatenable[A]) extends PiecewiseIntegrableTrajectory[A] {

	private val pieceIndexByStartingInstant: java.util.TreeMap[Instant, Int] = {
		val treeMap = new java.util.TreeMap[Instant, Int]()
		for index <- byPieceIndex.indices
			do treeMap.put(this.byPieceIndex(index).start, index)
		treeMap
	}

	override def integrate(from: Instant, to: Instant): A = {
		assert(from <= to)

		@tailrec
		def loop(stepIndex: Int, fragmentStart: Instant, accum: A): A = {
			if stepIndex >= byPieceIndex.size then throw new IllegalArgumentException(
				s"The trajectory is not defined in some part of the interval: from:$from, to:$to, byPieceIndex:$byPieceIndex"
			)
			val step = byPieceIndex(stepIndex)
			if to <= step.end
			then accum ++ step.integral(fragmentStart, to)
			else {
				val stepIntegral =
					if fragmentStart == step.start
					then step.wholeIntegral
					else step.integral(fragmentStart, step.end)
				loop(stepIndex - 1, step.start, accum ++ stepIntegral)
			}
		}

		val firstInvolvedStepEntry = pieceIndexByStartingInstant.floorEntry(from)
		val firstInvolvedStepIndex =
			if firstInvolvedStepEntry == null
			then byPieceIndex.size
			else firstInvolvedStepEntry.getValue
		loop(firstInvolvedStepIndex, from, concatenableOpsForA.empty)
	}
	//	override def byLinearIntervalIndex: immutable.IndexedSeq[StaggedInterval[A]] = steps
}
