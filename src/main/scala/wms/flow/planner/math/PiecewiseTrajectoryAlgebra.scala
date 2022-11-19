package wms.flow.planner
package math

import scala.collection.immutable
import time.Instant
import queue.Concatenable

import scala.annotation.targetName

object PiecewiseTrajectoryAlgebra {
	
	/** A piece of the trajectory between two consecutive inflection points. */
	trait Piece[+A] {
		/** The instant at which this piece starts. Should be equal to previous piece `end`. */
		def start: Instant

		/** The instant at which this piece ends. Should be equal to next piece `start` */
		def end: Instant

		/** Equivalent to {{{integral(this.start, this.end)}}} */
		def wholeIntegral: A

		/** Calculates the definite integral of upstream.
		 * @param from should be >= than [[#start]].
		 * @param to should be <= than [[#end]].
		 */
		def integral(from: Instant, to: Instant): A
	}
	
}

trait PiecewiseTrajectoryAlgebra[+A] {
	import PiecewiseTrajectoryAlgebra.*

	type PT[+B] <: PiecewiseTrajectory[B]

	def buildTrajectory[B: Fractionable : Concatenable](f: A => B): PT[B]
	def combine[B: Fractionable : Concatenable, C: Fractionable : Concatenable, D: Fractionable : Concatenable](
		ptA: PT[B],
		ptB: PT[C]
	)(f: (B, C) => D): PT[D]

	/** A piecewise trajectory of which we just need to know how to compute the definite integral on any interval. */
	trait PiecewiseTrajectory[+B: Fractionable : Concatenable] {
		self: PT[B] =>

		def getPieceAt(index: Int): Piece[B]

		def integrate(from: Instant, to: Instant): B

		def combineWith[C: Fractionable : Concatenable, D: Fractionable : Concatenable](other: PT[C])(f: (B, C) => D): PT[D] =
			combine[B, C, D](self, other)(f)
	}
}