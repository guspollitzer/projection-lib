package wms.flow.planner
package math

import time.*
import PiecewiseIntegrableTrajectory.Piece
import StaggeredIntegrableTrajectoryFactory.Step
import queue.Concatenable

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

object StaggeredIntegrableTrajectoryFactory {

	case class Step[A](start: Instant, end: Instant, wholeIntegral: A)

	case class PieceImpl[A, B: Fractionable](step: Step[A])(f: A => B) extends Piece[B] {
		override def integral(from: Instant, to: Instant): B =
			assert(from >= start && to <= end)
			f(step.wholeIntegral).takeFraction((to - from) / (end - start))

		override def start: Instant = step.start

		override def end: Instant = step.end

		override def wholeIntegral: B = f(step.wholeIntegral)
	}

}

case class StaggeredIntegrableTrajectoryFactory[A](steps: IndexedSeq[Step[A]]) {

	import StaggeredIntegrableTrajectoryFactory.*

	/** Maps steps starting instant to the index of the step in the [[steps]] [[IndexedSeq]].
	  * Implementation note: The map is constructed using the [[java.util.TreeMap]]s constructor that takes a
	  * [[java.util.SortedMap]] in order the tree is balanced. */
	private val stepIndexByStartingInstant: java.util.NavigableMap[Instant, Int] = {
		val tempSortedMap: java.util.SortedMap[Instant, Int] = new java.util.TreeMap()
		for index <- steps.indices
			do tempSortedMap.put(this.steps(index).start, index)
		new java.util.TreeMap(tempSortedMap)
	}

	def buildStaggeredIntegrableTrajectory[B: Fractionable](f: A => B)
		(using concatenableOpsForA: Concatenable[B]): PiecewiseIntegrableTrajectory[B] = StaggeredIntegrableTrajectory(f)

	private case class StaggeredIntegrableTrajectory[B: Fractionable](f: A => B)(using concatenationOpsForB: Concatenable[B])
		extends PiecewiseIntegrableTrajectory[B] {
		override def getPieceAt(index: Int): Piece[B] = new PieceImpl[A, B](steps(index))(f)

		override def integrate(from: Instant, to: Instant): B = {
			assert(from <= to)

			@tailrec
			def loop(stepIndex: Int, fragmentStart: Instant, accum: B): B = {
				if stepIndex >= steps.size then throw new IllegalArgumentException(
					s"The trajectory is not defined in some part of the interval: from:$from, to:$to, byPieceIndex:$steps"
				)
				val step = steps(stepIndex)
				val pieceWholeIntegral: B = f(step.wholeIntegral)
				if to <= step.end
				then {
					val stepIntegral = pieceWholeIntegral.takeFraction((to - fragmentStart) / (step.end - step.start))
					accum ++ stepIntegral
				}
				else {
					val stepIntegral =
						if fragmentStart == step.start
						then pieceWholeIntegral
						else pieceWholeIntegral.takeFraction((step.end - fragmentStart) / (step.end - step.start))
					loop(stepIndex + 1, step.start, accum ++ stepIntegral)
				}
			}

			val firstInvolvedStepEntry = stepIndexByStartingInstant.floorEntry(from)
			val firstInvolvedStepIndex =
				if firstInvolvedStepEntry == null
				then steps.size
				else firstInvolvedStepEntry.getValue
			loop(firstInvolvedStepIndex, from, concatenationOpsForB.empty)
		}
	}

}
