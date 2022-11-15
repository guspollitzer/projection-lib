package wms.flow.planner
package math

import scala.collection.immutable

import time.Instant

object PiecewiseIntegrableTrajectory {
	
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
/** A piecewise trajectory of which we just need to know how to compute the definite integral on any interval.  */
trait PiecewiseIntegrableTrajectory[+A] {
	import PiecewiseIntegrableTrajectory.*

	def byPieceIndex: immutable.IndexedSeq[Piece[A]]
//	def pieceIndexByStartingInstant: java.util.TreeMap[Instant, Int]

	def integrate(from: Instant, to: Instant): A
}