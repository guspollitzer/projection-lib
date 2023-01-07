package wms.flow.planner
package math

import global.*
import time.*
import util.TypeId

trait PiecewiseAlgebra {

	trait Trajectory[+A] {
		/** Gives the integral of the trajectory at the specified piece.
		  * Valid for integration-friendly trajectories. */
		def getWholePieceIntegralAt(index: PieceIndex): A
		
		/** Gives the mean value of the trajectory at the specified piece.
		  * Valid for non integration-friendly trajectories.  */
		def getPieceMeanAt(index: PieceIndex): A = getWholePieceIntegralAt(index)

		def integrate(from: Instant, to: Instant, extrapolate: Boolean)(using aTypeId: TypeId[A]): A

		def map[B](f: A => B): Trajectory[B]

		def richMap[B](f: (index: PieceIndex, startingInstant: Instant, endingInstant: Instant, wholePieceIntegral: A) => B): Trajectory[B]

		def combineWith[B, C](that: Trajectory[B])(f: (A, B) => C): Trajectory[C] = combine(this, that)(f)

		def richCombineWith[B, C](that: Trajectory[B])(f: (index: PieceIndex, startingInstant: Instant, endingInstant: Instant, a:A, b: B) => C): Trajectory[C] = richCombine(this, that)(f)

	}

	def numberOfPieces: Int
	def firstPieceStartingInstant: Instant

	/** Applies the function `f` to the initialState and the first piece of this [[PiecewiseAlgebra]]. Then applies `f` to the result and the next piece. This loop continues until the last piece of the until predicate gives true. */
	def reduceUntil[S](initialState: S, until: S => Boolean)(f: (state: S, index: PieceIndex, start: Instant, end: Instant) => S): S

	def buildTrajectory[A](f: (pieceIndex: PieceIndex) => A): Trajectory[A]

	def buildTrajectory[A](f: (pieceIndex: PieceIndex, pieceStart: Instant, pieceEnd: Instant) => A): Trajectory[A]

	def buildTrajectory[S, A](initialState: S)(valueBuilder: (state: S, index: PieceIndex, start: Instant, end: Instant) => A)(nextPieceInitialStateBuilder: (S, A) => S): Trajectory[A]

	def buildLazyTrajectory[S, A](initialState: S)(valueBuilder: (state: S, index: PieceIndex, start: Instant, end: Instant) => A)(nextPieceInitialStateBuilder: (S, A) => S): Trajectory[A]
	
	def combine[A, B, C](ta: Trajectory[A], tb: Trajectory[B])(f: (A, B) => C): Trajectory[C]

	def richCombine[A, B, C](ta: Trajectory[A], tb: Trajectory[B])(f: (index: PieceIndex, startingInstant: Instant, endingInstant: Instant, a: A, b: B) => C): Trajectory[C]
}
