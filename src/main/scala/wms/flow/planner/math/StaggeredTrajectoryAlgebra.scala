package wms.flow.planner
package math

import time.*
import PiecewiseTrajectoryAlgebra.{Helper, Piece}
import StaggeredTrajectoryAlgebra.Step
import queue.Concatenable
import StaggeredTrajectoryAlgebra.*

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.IndexedSeqView
import util.TypeId

object StaggeredTrajectoryAlgebra {

	case class Step[+A](start: Instant, end: Instant, wholeIntegral: A) extends Piece[A]

	case class FractionAndConcatOpsFor[A](fractionable: Fractionable[A], concatenable: Concatenable[A])
		(using val typeMark: TypeId[A])

	class FractionAndConcatOpsSummoner(opsByType: FractionAndConcatOpsFor[?]*) {
		def opsFor[A: TypeId]: FractionAndConcatOpsFor[A] = {
			val typeMarkA = summon[TypeId[A]]
			opsByType
				.find(facof => facof.typeMark.equalsTo(typeMarkA))
				.getOrElse(throw new IllegalArgumentException(s"A Fractionable for $typeMarkA was not found."))
				.asInstanceOf[FractionAndConcatOpsFor[A]]
		}

		def summonFractionableFor[A: TypeId]: Fractionable[A] = opsFor[A].fractionable

		def summonConcatenableFor[A: TypeId]: Concatenable[A] = opsFor[A].concatenable
	}
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
)(using opsSummoner: FractionAndConcatOpsSummoner) extends PiecewiseTrajectoryAlgebra[A] {
	selfFactory =>

	import StaggeredTrajectoryAlgebra.*

	def this(steps: IndexedSeqView[Step[A]])(using opsSummoner: FractionAndConcatOpsSummoner) =
		this(
		steps,
		{
			val tempSortedMap: java.util.SortedMap[Instant, Int] = new java.util.TreeMap()
			for index <- steps.indices
				do tempSortedMap.put(steps(index).start, index)
			new java.util.TreeMap(tempSortedMap)
		}
		)

	case class StepView[+B](stepIndex: Int)(wholeIntegralByStepIndex: Int => B) extends Piece[B] {

		override def start: Instant = steps(stepIndex).start

		override def end: Instant = steps(stepIndex).end

		override def wholeIntegral: B = wholeIntegralByStepIndex(stepIndex)
	}

	type P[+B] = StepView[B]
	type T[+B] = StaggeredTrajectory[B]

	override def getPieceAt(index: Int): Step[A] = steps(index)

	override def map[B](f: A => B): StaggeredTrajectoryAlgebra[B] =
		new StaggeredTrajectoryAlgebra[B](
			steps.map(step => Step(step.start, step.end, f(step.wholeIntegral))),
			stepIndexByStartingInstant
		)

	override def buildTrajectory[B: TypeId](wholeIntegralByStepIndex: Int => B): T[B] =
		new StaggeredTrajectory(wholeIntegralByStepIndex)

	import math.PiecewiseTrajectoryAlgebra

	override def combine[B: TypeId, C: TypeId, D: TypeId](ptA: T[B], ptB: T[C])(f: (B, C) => D): T[D] =
		selfFactory.buildTrajectory[D](stepIndex => f(
			ptA.wholeIntegralByStepIndex(stepIndex),
			ptB.wholeIntegralByStepIndex(stepIndex)
		)
		)

	class StaggeredTrajectory[+B: TypeId](val wholeIntegralByStepIndex: Int => B) extends PiecewiseTrajectory[B] {
		selfTrajectory =>

		override def getPieceAt(index: Int): StepView[B] = {
			StepView[B](index)(wholeIntegralByStepIndex)
		}

		override def integrate(from: Instant, to: Instant): B = {
			assert(from <= to)

			given concatOps: Concatenable[B] = opsSummoner.summonConcatenableFor[B]
			given factionOps: Fractionable[B] = opsSummoner.summonFractionableFor[B]

			@tailrec
			def loop(stepIndex: Int, fragmentStart: Instant, accum: B): B = {
				if stepIndex >= steps.size then throw new IllegalArgumentException(
					s"The trajectory is not defined in some part of the interval: from:$from, to:$to, byPieceIndex:$steps"
				)
				val step = steps(stepIndex)
				val pieceWholeIntegral: B = wholeIntegralByStepIndex(stepIndex)
				if to <= step.end
				then {
					val stepIntegral = pieceWholeIntegral.takeFraction((to - fragmentStart) / (step.end - step.start)
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
			loop(firstInvolvedStepIndex, from, concatOps.empty)
		}
	}


}
