package wms.flow.planner
package math

import math.{Fractionable, PiecewiseAlgebra}
import queue.Concatenable
import util.TypeId
import time.*

import scala.annotation.tailrec
import scala.IArray


object StaggeredAlgebra {

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

import StaggeredAlgebra.*

class StaggeredAlgebra private(
	firstPieceStartingInstant: Instant,
	pieceEndingInstantByIndex: IArray[Instant],
	pieceIndexByEndingInstant: java.util.NavigableMap[Instant, Int]
)(
	using opsSummoner: FractionAndConcatOpsSummoner
) extends PiecewiseAlgebra {

	import StaggeredAlgebra.*


	def this(firstPieceStartingInstant: Instant, pieceEndingInstantByIndex: IArray[Instant])(using opsSummoner: StaggeredAlgebra.FractionAndConcatOpsSummoner) =
		this(
		firstPieceStartingInstant: Instant,
		pieceEndingInstantByIndex,
		{
			val tempSortedMap: java.util.SortedMap[Instant, Int] = new java.util.TreeMap()
			for index <- pieceEndingInstantByIndex.indices
				do tempSortedMap.put(pieceEndingInstantByIndex(index), index)
			new java.util.TreeMap(tempSortedMap)
		}
		)

	abstract class StaggeredTrajectory[+A: TypeId] extends Trajectory[A] {

		override def integrate(from: Instant, to: Instant): A = {
			assert(from <= to)

			given concatOps: Concatenable[A] = opsSummoner.summonConcatenableFor[A]

			given factionOps: Fractionable[A] = opsSummoner.summonFractionableFor[A]

			@tailrec
			def loop(pieceIndex: Int, fragmentStart: Instant, accum: A): A = {
				if pieceIndex < 0 || numberOfPieces <= pieceIndex then throw new IllegalArgumentException(
					s"The trajectory is not defined in some part of the interval: pieceIndex:$pieceIndex, from:$from, to:$to, firstPieceStartingInstant:$firstPieceStartingInstant, lastPieceEndingInstant:${getWholePieceIntegralAt(numberOfPieces-1)}"
				)
				val wholePieceIntegral = getWholePieceIntegralAt(pieceIndex)
				val pieceStart = pieceEndingInstantByIndex(pieceIndex)
				val pieceEnd = pieceEndingInstantByIndex(pieceIndex + 1)
				if to <= pieceEnd
				then {
					val pieceIntegral = wholePieceIntegral.takeFraction((to - fragmentStart) / (pieceEnd - pieceStart))
					accum ++ pieceIntegral
				}
				else {
					val pieceIntegral =
						if fragmentStart == pieceStart
						then wholePieceIntegral
						else wholePieceIntegral.takeFraction((pieceEnd - fragmentStart) / (pieceEnd - pieceStart))
					loop(pieceIndex + 1, pieceStart, accum ++ pieceIntegral)
				}
			}

			val firstInvolvedPieceEntry = pieceIndexByEndingInstant.higherEntry(from)
			val firstInvolvedPieceIndex =
				if firstInvolvedPieceEntry == null
				then numberOfPieces
				else if from < firstPieceStartingInstant
					 then -1
					 else firstInvolvedPieceEntry.getValue
			loop(firstInvolvedPieceIndex, from, concatOps.empty)
		}

		override def map[B: TypeId](f: A => B): StaggeredTrajectory[B] = new LazyOne(this)(f)

		override def richMap[B: TypeId](f: (index: Int, startingInstant: Instant, endingInstant: Instant, wholePieceIntegral: A) => B): StaggeredTrajectory[B] = {
			new RichOne(this)(f)
		}
	}

	protected class Eager[+A: TypeId](wholePieceIntegralByIndex: IndexedSeq[A]) extends StaggeredTrajectory[A] {
		override def numberOfPieces: Int = wholePieceIntegralByIndex.size

		override def getWholePieceIntegralAt(index: Int): A = wholePieceIntegralByIndex(index)
	}

	protected class LazyOne[+A, +B: TypeId](base: Trajectory[A])(func: A => B) extends StaggeredTrajectory[B] {
		override def numberOfPieces: Int = base.numberOfPieces

		override def getWholePieceIntegralAt(index: Int): B = func(base.getWholePieceIntegralAt(index))
	}

	protected class LazyTwo[+A, +B, +C: TypeId](ta: Trajectory[A], tb: Trajectory[B])(biFunc: (A, B) => C) extends StaggeredTrajectory[C] {
		override def numberOfPieces: Int = ta.numberOfPieces

		override def getWholePieceIntegralAt(index: Int): C = biFunc(ta.getWholePieceIntegralAt(index), tb.getWholePieceIntegralAt(index))
	}

	protected class RichOne[+A, +B: TypeId](base: Trajectory[A])(multiFunc: (index: Int, startingInstant: Instant, endingInstant: Instant, wholePieceIntegral: A) => B) extends StaggeredTrajectory[B] {
		override def numberOfPieces: Int = base.numberOfPieces

		override def getWholePieceIntegralAt(index: Int): B = {
			val startingInstant = if index == 0 then firstPieceStartingInstant else pieceEndingInstantByIndex(index - 1)
			val endingInstant = pieceEndingInstantByIndex(index)
			multiFunc(index, startingInstant, endingInstant, base.getWholePieceIntegralAt(index))
		}
	}

	protected class RichTwo[+A, +B, +C: TypeId](ta: Trajectory[A], tb: Trajectory[B])(multiFunc: (index: Int, startingInstant: Instant, endingInstant: Instant, a: A, b: B) => C) extends StaggeredTrajectory[C] {
		override def numberOfPieces: Int = ta.numberOfPieces

		override def getWholePieceIntegralAt(index: Int): C = {
			val startingInstant = if index == 0 then firstPieceStartingInstant else pieceEndingInstantByIndex(index - 1)
			val endingInstant = pieceEndingInstantByIndex(index)
			multiFunc(index, startingInstant, endingInstant, ta.getWholePieceIntegralAt(index), tb.getWholePieceIntegralAt(index))
		}
	}

//	override type T[+A] = StaggeredTrajectory[A]

	override def buildTrajectory[A: TypeId](wholeIntegralByPieceIndex: IterableOnce[A]): StaggeredTrajectory[A] = {
		wholeIntegralByPieceIndex match {
			case is: IndexedSeq[A] => new Eager(is)
			case io => new Eager(io.iterator.toIndexedSeq)
		}
	}

	override def combine[A, B, C: TypeId](ta: Trajectory[A], tb: Trajectory[B])(f: (A, B) => C): StaggeredTrajectory[C] = new LazyTwo(ta, tb)(f)

	override def richCombine[A, B, C: TypeId](ta: Trajectory[A], tb: Trajectory[B])(f: (index: Int, startingInstant: Instant, endingInstant: Instant, a: A, b: B) => C): StaggeredTrajectory[C] =
		new RichTwo(ta, tb)(f)

}
