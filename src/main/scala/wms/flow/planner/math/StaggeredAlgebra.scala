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
		(using val typeId: TypeId[A])

	class FractionAndConcatOpsSummoner(opsByType: FractionAndConcatOpsFor[?]*) {
		def opsFor[A: TypeId]: FractionAndConcatOpsFor[A] = {
			val typeMarkA = summon[TypeId[A]]
			opsByType
				.find(facof => facof.typeId.equalsTo(typeMarkA))
				.getOrElse(throw new IllegalArgumentException(s"A Fractionable for $typeMarkA was not found."))
				.asInstanceOf[FractionAndConcatOpsFor[A]]
		}

		def summonFractionableFor[A: TypeId]: Fractionable[A] = opsFor[A].fractionable

		def summonConcatenableFor[A: TypeId]: Concatenable[A] = opsFor[A].concatenable
	}
}

import StaggeredAlgebra.*

class StaggeredAlgebra private(
	val firstPieceStartingInstant: Instant,
	pieceEndingInstantByIndex: IArray[Instant],
	pieceIndexByEndingInstant: java.util.NavigableMap[Instant, Int]
)(using facOpsSummoner: FractionAndConcatOpsSummoner) extends PiecewiseAlgebra {

	def this(firstPieceStartingInstant: Instant, pieceEndingInstantByIndex: IArray[Instant])(using facOpsSummoner: FractionAndConcatOpsSummoner) =
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

	def getStartOfPiece(pieceIndex: Int): Instant =
		if pieceIndex == 0 then firstPieceStartingInstant
		else if pieceIndex >= numberOfPieces then pieceEndingInstantByIndex(numberOfPieces - 1)
			 else pieceEndingInstantByIndex(pieceIndex - 1)

	abstract class StaggeredTrajectory[+A] extends Trajectory[A] {

		override def integrate(from: Instant, to: Instant, extrapolate: Boolean)(using aTypeId: TypeId[A]): A = {
			assert(from <= to)

			given concatOps: Concatenable[A] = facOpsSummoner.summonConcatenableFor[A]

			given fractionOps: Fractionable[A] = facOpsSummoner.summonFractionableFor[A]

			@tailrec
			def loop(pieceIndex: Int, pieceStart: Instant, fragmentStart: Instant, accum: A): A = {
				if extrapolate && pieceIndex == numberOfPieces then {
					val pieceEnd = pieceStart
					val lastPieceWholePieceIntegral = getWholePieceIntegralAt(numberOfPieces - 1)
					accum ++ lastPieceWholePieceIntegral.takeFraction((to - fragmentStart)/(pieceEnd - getStartOfPiece(numberOfPieces - 1) ))
				} else if pieceIndex < 0 || numberOfPieces <= pieceIndex then {
					throw new IllegalArgumentException(
						s"The trajectory is not defined in some part of the interval: pieceIndex:$pieceIndex, from:$from, to:$to, extrapolate:$extrapolate, firstPieceStartingInstant:$firstPieceStartingInstant, lastPieceEndingInstant:${getWholePieceIntegralAt(numberOfPieces-1)}"
					)
				} else {
					val wholePieceIntegral = getWholePieceIntegralAt(pieceIndex)
					val pieceEnd = pieceEndingInstantByIndex(pieceIndex)
					if to <= pieceEnd
					then {
						val pieceIntegral = wholePieceIntegral.takeFraction((to - fragmentStart) / (pieceEnd - pieceStart))
						accum ++ pieceIntegral;
					}
					else {
						val pieceIntegral =
							if fragmentStart == pieceStart
							then wholePieceIntegral
							else wholePieceIntegral.takeFraction((pieceEnd - fragmentStart) / (pieceEnd - pieceStart));
						loop(pieceIndex + 1, pieceEnd, pieceEnd, accum ++ pieceIntegral);
					}
				}
			}

			if numberOfPieces == 0 then concatOps.empty
			else {
				val firstInvolvedPieceEntry = pieceIndexByEndingInstant.higherEntry(from)
				val firstInvolvedPieceIndex =
					if firstInvolvedPieceEntry == null
					then numberOfPieces
					else if from < firstPieceStartingInstant
						 then -1
						 else firstInvolvedPieceEntry.getValue;
				val firstInvolvedPieceStart = getStartOfPiece(firstInvolvedPieceIndex);
				loop(firstInvolvedPieceIndex, firstInvolvedPieceStart, from, concatOps.empty)
			}
		}

		override def map[B](f: A => B): StaggeredTrajectory[B] = new MapOne(this)(f)

		override def richMap[B](f: (index: Int, startingInstant: Instant, endingInstant: Instant, wholePieceIntegral: A) => B): StaggeredTrajectory[B] = {
			new RichOne(this)(f)
		}
	}

	protected class Eager[+A](wholePieceIntegralByIndex: IndexedSeq[A]) extends StaggeredTrajectory[A] {

		override def getWholePieceIntegralAt(index: Int): A = wholePieceIntegralByIndex(index)
	}

	protected class Lazy[+A](func: (pieceIndex: Int) => A) extends StaggeredTrajectory[A] {

		override def getWholePieceIntegralAt(index: Int): A = func(index)
	}

	protected class MapOne[+A, +B](base: Trajectory[A])(func: A => B) extends StaggeredTrajectory[B] {

		override def getWholePieceIntegralAt(index: Int): B = func(base.getWholePieceIntegralAt(index))
	}

	protected class MapTwo[+A, +B, +C](ta: Trajectory[A], tb: Trajectory[B])(biFunc: (A, B) => C) extends StaggeredTrajectory[C] {

		override def getWholePieceIntegralAt(index: Int): C = biFunc(ta.getWholePieceIntegralAt(index), tb.getWholePieceIntegralAt(index))
	}

	protected class RichOne[+A, +B](base: Trajectory[A])(multiFunc: (index: Int, startingInstant: Instant, endingInstant: Instant, wholePieceIntegral: A) => B) extends StaggeredTrajectory[B] {

		override def getWholePieceIntegralAt(index: Int): B = {
			val startingInstant = if index == 0 then firstPieceStartingInstant else pieceEndingInstantByIndex(index - 1)
			val endingInstant = pieceEndingInstantByIndex(index)
			multiFunc(index, startingInstant, endingInstant, base.getWholePieceIntegralAt(index))
		}
	}

	protected class RichTwo[+A, +B, +C](ta: Trajectory[A], tb: Trajectory[B])(multiFunc: (index: Int, startingInstant: Instant, endingInstant: Instant, a: A, b: B) => C) extends StaggeredTrajectory[C] {

		override def getWholePieceIntegralAt(index: Int): C = {
			val startingInstant = if index == 0 then firstPieceStartingInstant else pieceEndingInstantByIndex(index - 1)
			val endingInstant = pieceEndingInstantByIndex(index)
			multiFunc(index, startingInstant, endingInstant, ta.getWholePieceIntegralAt(index), tb.getWholePieceIntegralAt(index))
		}
	}

	def numberOfPieces: Int = pieceEndingInstantByIndex.size

	def buildTrajectory[A](f: (pieceIndex: Int) => A): Trajectory[A] = new Lazy[A](f)

	def buildTrajectory[A](f: (pieceIndex: Int, pieceStart: Instant, pieceEnd: Instant) => A): Trajectory[A] = {
		new Lazy[A](pieceIndex => {
			val pieceStart = if pieceIndex == 0 then firstPieceStartingInstant else pieceEndingInstantByIndex(pieceIndex - 1)
			val pieceEnd = pieceEndingInstantByIndex(pieceIndex)
			f(pieceIndex, pieceStart, pieceEnd)
		})
	}

	def buildTrajectory[S, A](initialState: S)(valueBuilder: (state: S, index: Int, start: Instant, end: Instant) => A)(nextStateBuilder: A => S): StaggeredTrajectory[A] = {
		val builder = IndexedSeq.newBuilder[A];

		@tailrec
		def loop(state: S, index: Int, start: Instant): Unit = {
			if index < pieceEndingInstantByIndex.size then {
				val end = pieceEndingInstantByIndex(index)
				val value = valueBuilder(state, index, start, end)
				builder.addOne(value)
				loop(nextStateBuilder(value), index + 1, end)
			}
		}

		loop(initialState, 0, firstPieceStartingInstant)
		new Eager[A](builder.result())
	}

	override def combine[A, B, C](ta: Trajectory[A], tb: Trajectory[B])(f: (A, B) => C): StaggeredTrajectory[C] = new MapTwo(ta, tb)(f)

	override def richCombine[A, B, C](ta: Trajectory[A], tb: Trajectory[B])(f: (index: Int, startingInstant: Instant, endingInstant: Instant, a: A, b: B) => C): StaggeredTrajectory[C] =
		new RichTwo(ta, tb)(f)

	override def reduceUntil[S](initialState: S, until: S => Boolean)(f: (state: S, index: Int, start: Instant, end: Instant) => S): S = {
		@tailrec
		def loop(state: S, index: Int, start: Instant): S = {
			if index == pieceEndingInstantByIndex.size || until(state) then state
			else {
				val end = pieceEndingInstantByIndex(index)
				val newState = f(state, index, start, end)
				loop(newState, index + 1, end)
			}
		}
		loop(initialState, 0, firstPieceStartingInstant)
	}
}
