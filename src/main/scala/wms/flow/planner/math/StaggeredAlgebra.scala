package wms.flow.planner
package math

import global.*
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
	pieceIndexByEndingInstant: java.util.NavigableMap[Instant, PieceIndex]
)(using facOpsSummoner: FractionAndConcatOpsSummoner) extends PiecewiseAlgebra {

	def this(firstPieceStartingInstant: Instant, pieceEndingInstantByIndex: IArray[Instant])(using facOpsSummoner: FractionAndConcatOpsSummoner) =
		this(
		firstPieceStartingInstant: Instant,
		pieceEndingInstantByIndex,
		{
			val tempSortedMap: java.util.SortedMap[Instant, PieceIndex] = new java.util.TreeMap()
			for index <- pieceEndingInstantByIndex.indices
				do tempSortedMap.put(pieceEndingInstantByIndex(index), index)
			new java.util.TreeMap(tempSortedMap)
		}
		)

	def getStartOfPiece(pieceIndex: PieceIndex): Instant =
		if pieceIndex == 0 then firstPieceStartingInstant
		else if pieceIndex >= numberOfPieces then pieceEndingInstantByIndex(numberOfPieces - 1)
			 else pieceEndingInstantByIndex(pieceIndex - 1)

	abstract class StaggeredTrajectory[+A] extends Trajectory[A] {

		override def integrate(from: Instant, to: Instant, extrapolateLastPiece: Boolean)(using aTypeId: TypeId[A]): A = {
			assert(from <= to)

			given concatOps: Concatenable[A] = facOpsSummoner.summonConcatenableFor[A]

			given fractionOps: Fractionable[A] = facOpsSummoner.summonFractionableFor[A]

			@tailrec
			def loop(pieceIndex: PieceIndex, pieceStart: Instant, fragmentStart: Instant, accum: A): A = {
				if extrapolateLastPiece && pieceIndex == numberOfPieces then {
					val pieceEnd = pieceStart
					val lastPieceWholePieceIntegral = getValueAt(numberOfPieces - 1)
					accum ++ lastPieceWholePieceIntegral.takeFraction((to - fragmentStart)/(pieceEnd - getStartOfPiece(numberOfPieces - 1) ))
				} else if pieceIndex < 0 || numberOfPieces <= pieceIndex then {
					throw new IllegalArgumentException(
						s"The trajectory is not defined in some part of the interval: pieceIndex:$pieceIndex, from:$from, to:$to, extrapolate:$extrapolateLastPiece, firstPieceStartingInstant:$firstPieceStartingInstant, lastPieceEndingInstant:${getValueAt(numberOfPieces-1)}"
					)
				} else {
					val wholePieceIntegral = getValueAt(pieceIndex)
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

		override def foreach(f: A => Unit): Unit = {
			for pieceIndex <- 0 until numberOfPieces do f(getValueAt(pieceIndex))
		}

		override def map[B](f: A => B): StaggeredTrajectory[B] = new MapOne(this)(f)

		override def richMap[B](f: (index: PieceIndex, startingInstant: Instant, endingInstant: Instant, wholePieceIntegral: A) => B): StaggeredTrajectory[B] = {
			new RichOne(this)(f)
		}
	}

	protected class Eager[+A](wholePieceIntegralByIndex: IndexedSeq[A]) extends StaggeredTrajectory[A] {

		override def getValueAt(index: PieceIndex): A = wholePieceIntegralByIndex(index)
	}

	protected class Lazy[+A](valueByPieceIndex: LazyList[A]) extends StaggeredTrajectory[A] {
		/** TODO Optimize this method: take advantage of what was done in the call with an index less than or equal to the one received now.  */
		override def getValueAt(index: PieceIndex): A = valueByPieceIndex(index)
	}

	protected class Deferred[+A](func: (pieceIndex: PieceIndex) => A) extends StaggeredTrajectory[A] {

		override def getValueAt(index: PieceIndex): A = func(index)
	}

	protected class MapOne[+A, +B](base: Trajectory[A])(func: A => B) extends StaggeredTrajectory[B] {

		override def getValueAt(index: PieceIndex): B = func(base.getValueAt(index))
	}

	protected class MapTwo[+A, +B, +C](ta: Trajectory[A], tb: Trajectory[B])(biFunc: (A, B) => C) extends StaggeredTrajectory[C] {

		override def getValueAt(index: PieceIndex): C = biFunc(ta.getValueAt(index), tb.getValueAt(index))
	}

	protected class RichOne[+A, +B](base: Trajectory[A])(multiFunc: (index: PieceIndex, startingInstant: Instant, endingInstant: Instant, wholePieceIntegral: A) => B) extends StaggeredTrajectory[B] {

		override def getValueAt(index: PieceIndex): B = {
			val startingInstant = if index == 0 then firstPieceStartingInstant else pieceEndingInstantByIndex(index - 1)
			val endingInstant = pieceEndingInstantByIndex(index)
			multiFunc(index, startingInstant, endingInstant, base.getValueAt(index))
		}
	}

	protected class RichTwo[+A, +B, +C](ta: Trajectory[A], tb: Trajectory[B])(multiFunc: (index: PieceIndex, startingInstant: Instant, endingInstant: Instant, a: A, b: B) => C) extends StaggeredTrajectory[C] {

		override def getValueAt(index: PieceIndex): C = {
			val startingInstant = if index == 0 then firstPieceStartingInstant else pieceEndingInstantByIndex(index - 1)
			val endingInstant = pieceEndingInstantByIndex(index)
			multiFunc(index, startingInstant, endingInstant, ta.getValueAt(index), tb.getValueAt(index))
		}
	}

	def numberOfPieces: Int = pieceEndingInstantByIndex.size

	def buildTrajectory[A](f: (pieceIndex: PieceIndex) => A): Trajectory[A] = new Deferred[A](f)

	def buildTrajectory[A](f: (pieceIndex: PieceIndex, pieceStart: Instant, pieceEnd: Instant) => A): Trajectory[A] = {
		new Deferred[A](pieceIndex => {
			val pieceStart = if pieceIndex == 0 then firstPieceStartingInstant else pieceEndingInstantByIndex(pieceIndex - 1)
			val pieceEnd = pieceEndingInstantByIndex(pieceIndex)
			f(pieceIndex, pieceStart, pieceEnd)
		})
	}

	def buildTrajectory[S, A](initialState: S)(valueBuilder: (state: S, index: PieceIndex, start: Instant, end: Instant) => A)(nextPieceInitialStateBuilder: (S, A) => S): StaggeredTrajectory[A] = {
		val builder = IndexedSeq.newBuilder[A];

		@tailrec
		def loop(state: S, index: PieceIndex, start: Instant): Unit = {
			if index < pieceEndingInstantByIndex.size then {
				val end = pieceEndingInstantByIndex(index)
				val value = valueBuilder(state, index, start, end)
				builder.addOne(value)
				loop(nextPieceInitialStateBuilder(state, value), index + 1, end)
			}
		}

		loop(initialState, 0, firstPieceStartingInstant)
		new Eager[A](builder.result())
	}

	def buildLazyTrajectory[S, A](initialState: S)(valueBuilder: (state: S, index: PieceIndex, start: Instant, end: Instant) => A)(nextPieceInitialStateBuilder: (S, A) => S): StaggeredTrajectory[A] = {
		def loop(state: S, index: PieceIndex, start: Instant): LazyList[A] = {
			if index < pieceEndingInstantByIndex.size then {
				val end = pieceEndingInstantByIndex(index)
				val value = valueBuilder(state, index, start, end);
				value #:: loop(nextPieceInitialStateBuilder(state, value), index + 1, end)
			} else {
				LazyList.empty
			}
		}
		new Lazy(loop(initialState, 0, firstPieceStartingInstant));
	}

	override def combine[A, B, C](ta: Trajectory[A], tb: Trajectory[B])(f: (A, B) => C): StaggeredTrajectory[C] = new MapTwo(ta, tb)(f)

	override def richCombine[A, B, C](ta: Trajectory[A], tb: Trajectory[B])(f: (index: PieceIndex, startingInstant: Instant, endingInstant: Instant, a: A, b: B) => C): StaggeredTrajectory[C] =
		new RichTwo(ta, tb)(f)

	override def reduceUntil[S](initialState: S, until: S => Boolean)(f: (state: S, index: PieceIndex, start: Instant, end: Instant) => S): S = {
		@tailrec
		def loop(state: S, index: PieceIndex, start: Instant): S = {
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
