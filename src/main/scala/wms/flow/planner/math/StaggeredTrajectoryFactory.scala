package wms.flow.planner
package math

import time.*
import PiecewiseTrajectoryAlgebra.Piece
import StaggeredTrajectoryFactory.Step
import queue.Concatenable

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.IndexedSeqView

object StaggeredTrajectoryFactory {

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

/**
  * @param steps the steps on which the trajectories built by this instance are based.
  *
  * @param stepIndexByStartingInstant Maps steps starting instant to the index of the step in the [[steps]]
  * [[IndexedSeq]]. This map is constructed using the [[java.util.TreeMap]]s
  * constructor that takes a [[java.util.SortedMap]] in order the tree is balanced.
  * */
class StaggeredTrajectoryFactory[A] private(
	steps: IndexedSeqView[Step[A]],
	stepIndexByStartingInstant: java.util.NavigableMap[Instant, Int]
) extends PiecewiseTrajectoryAlgebra[A] {
	selfFactory =>

	import StaggeredTrajectoryFactory.*

	type PT[+B] = StaggeredTrajectory[B]

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

	def map[B](f: A => B): StaggeredTrajectoryFactory[B] =
		new StaggeredTrajectoryFactory[B](
			steps.view.map(step => Step(step.start, step.end, f(step.wholeIntegral))),
			stepIndexByStartingInstant
		)

	def buildTrajectory[B: Fractionable : Concatenable](f: A => B): PT[B] =
		new StaggeredTrajectory(f)

	import math.PiecewiseTrajectoryAlgebra

	override def combine[B: Fractionable : Concatenable, C: Fractionable : Concatenable, D: Fractionable : Concatenable](
		ptA: PT[B],
		ptB: PT[C]
	)(f: (B, C) => D): PT[D] = selfFactory.buildTrajectory[D](a => f(ptA.f(a), ptB.f(a)))

	class StaggeredTrajectory[+B: Fractionable](val f: A => B)(using concatenationOpsForB: Concatenable[B])
		extends PiecewiseTrajectory[B] {
		selfTrajectory =>

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

//		override def combineWith[C: Fractionable : Concatenable, D: Fractionable : Concatenable](other: StaggeredTrajectory[C])
//			(f: (B, C) => D): PiecewiseTrajectory[D] = combine(this, other)(f)

	}
}
