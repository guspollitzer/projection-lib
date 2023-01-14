package wms.flow.planner
package math

import global.*
import time.*
import util.TypeId

trait PiecewiseAlgebra {

	trait Trajectory[+A] {

		/** Gives the value of the trajectory at the specified piece.  */
		def getValueAt(index: PieceIndex): A

		/** Calculates the definite integral of this trajectory in the specified interval assuming the value at each piece represents the definite integral of the trajectory on said piece.
		  * For example, if the values at each piece of this trajectory represented the value at said piece of the staggered function we want to integrate, then we first have to map said values to whole piece integrals like this:
		  * {{{ this.richMap { (_, start, end, value) => value * (end - start) }.integrate(from, to, extrapolate) } }}}
		  *
		  * @param from the lower limit of the interval on which to integrate
		  * @param to the upper limit of the interval on which to integrate
		  * @param extrapolateLastPiece when true the interval bounds may be greater than the end of the last piece of this piecewise trajectory, and the result is the same than the one of a trajectory equal to this but with the last piece repeated indefinitely. */
		def integrate(from: Instant, to: Instant, extrapolateLastPiece: Boolean)(using aTypeId: TypeId[A]): A

		def foreach(f: A => Unit): Unit

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
