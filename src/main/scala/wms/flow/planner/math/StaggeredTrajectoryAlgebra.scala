package wms.flow.planner
package math

import time.*
import PiecewiseTrajectoryAlgebra.Piece
import StaggeredTrajectoryAlgebra.Step
import queue.Concatenable

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.IndexedSeqView

object StaggeredTrajectoryAlgebra {

	case class Step[+A](start: Instant, end: Instant, wholeIntegral: A) extends Piece[A]
}

/**
  * @param steps the steps on which the trajectories built by this instance are based.
  *
  * @param stepIndexByStartingInstant Maps steps starting instant to the index of the step in the [[steps]]
  * [[IndexedSeq]]. This map is constructed using the [[java.util.TreeMap]]s
  * constructor that takes a [[java.util.SortedMap]] in order the tree is balanced.
  * */
class StaggeredTrajectoryAlgebra[+A] private(
	steps: IndexedSeqView[Step[A]],
	stepIndexByStartingInstant: java.util.NavigableMap[Instant, Int]
) extends PiecewiseTrajectoryAlgebra[A] {
	selfFactory =>

	import StaggeredTrajectoryAlgebra.*

	def this(steps: IndexedSeqView[Step[A]]) =
		this(
		steps,
		{
			val tempSortedMap: java.util.SortedMap[Instant, Int] = new java.util.TreeMap()
			for index <- steps.indices
				do tempSortedMap.put(steps(index).start, index)
			new java.util.TreeMap(tempSortedMap)
		}
		)

	case class StepView[+B: Fractionable](stepIndex: Int)(wholeIntegralByStepIndex: Int => B) extends Piece[B] {

		override def start: Instant = steps(stepIndex).start

		override def end: Instant = steps(stepIndex).end

		override def wholeIntegral: B = wholeIntegralByStepIndex(stepIndex)
	}

	type P[+B] = StepView[B]
	type T[+B] = StaggeredTrajectory[B]

	override def getPieceAt(index: Int): Step[A] = steps(index)

	override def map[B](f:A => B): StaggeredTrajectoryAlgebra[B] =
		new StaggeredTrajectoryAlgebra[B](
			steps.view.map(step => Step(step.start, step.end, f(step.wholeIntegral))),
			stepIndexByStartingInstant
		)

	override def buildTrajectory[B: Fractionable : Concatenable](wholeIntegralByStepIndex: Int => B): T[B] =
		new StaggeredTrajectory(wholeIntegralByStepIndex)

	import math.PiecewiseTrajectoryAlgebra

	override def combine[B: Fractionable : Concatenable, C: Fractionable : Concatenable, D: Fractionable : Concatenable](
		ptA: T[B],
		ptB: T[C]
	)(f: (B, C) => D): T[D] = selfFactory.buildTrajectory[D](stepIndex => f(ptA.wholeIntegralByStepIndex(stepIndex), ptB.wholeIntegralByStepIndex(stepIndex)))

	class StaggeredTrajectory[+B: Fractionable](val wholeIntegralByStepIndex: Int => B)(using concatenationOpsForB: Concatenable[B])
		extends PiecewiseTrajectory[B] {
		selfTrajectory =>

		override def getPieceAt(index: Int): StepView[B] = {
			StepView[B](index)(wholeIntegralByStepIndex)
		}

		override def integrate(from: Instant, to: Instant): B = {
			assert(from <= to)

			@tailrec
			def loop(stepIndex: Int, fragmentStart: Instant, accum: B): B = {
				if stepIndex >= steps.size then throw new IllegalArgumentException(
					s"The trajectory is not defined in some part of the interval: from:$from, to:$to, byPieceIndex:$steps"
				)
				val step = steps(stepIndex)
				val pieceWholeIntegral: B = wholeIntegralByStepIndex(stepIndex)
				if to <= step.end
				then {
					val stepIntegral = pieceWholeIntegral.takeFraction((to - fragmentStart) / (step.end - step
						.start)
					)
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
