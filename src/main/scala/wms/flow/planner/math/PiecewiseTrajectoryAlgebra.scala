package wms.flow.planner
package math

import scala.collection.immutable
import time.Instant
import queue.Concatenable

import scala.annotation.targetName
import util.TypeId

object PiecewiseTrajectoryAlgebra {
	
	/** A piece of the trajectory between two consecutive inflection points. */
	trait Piece[+A] {
		/** The instant at which this piece starts. Should be equal to previous piece `end`. */
		def start: Instant

		/** The instant at which this piece ends. Should be equal to next piece `start` */
		def end: Instant

		/** Equivalent to {{{integral(this.start, this.end)}}} */
		def wholeIntegral: A
//
//		/** Calculates the definite integral of upstream.
//		 * @param from should be >= than [[#start]].
//		 * @param to should be <= than [[#end]].
//		 */
//		def integral(from: Instant, to: Instant): A
	}

	trait Helper[B]
	
}

trait PiecewiseTrajectoryAlgebra[+A] {
	import PiecewiseTrajectoryAlgebra.*

	type P[+B] <: Piece[B]
	type T[+B] <: PiecewiseTrajectory[B]

	def getPieceAt(index: Int): Piece[A]

	def map[B](f: A => B): PiecewiseTrajectoryAlgebra[B]

	def buildTrajectory[B: TypeId](f: Int => B): T[B]

	def combine[B: TypeId, C: TypeId, D: TypeId](ptA: T[B], ptB: T[C])(f: (B, C) => D): T[D]

	/** A piecewise trajectory of which we just need to know how to compute the definite integral on any interval. */
	trait PiecewiseTrajectory[+B: TypeId] {
		self: T[B] =>

		def getPieceAt(index: Int): P[B]

		def integrate(from: Instant, to: Instant, extrapolate: Boolean): B

		def combineWith[C: TypeId, D: TypeId](other: T[C])(f: (B, C) => D): T[D] = combine[B, C, D](self, other)(f)
	}
}