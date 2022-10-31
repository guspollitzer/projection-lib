package wms.flow.planner
package math

import scala.collection.immutable

import time.Instant

object PiecewiseIntegrableTrajectory {
	
	trait LinearInterval[+A] {
		/** Should be equal to previous interval end. */
		def start: Instant

		/** Should be equal to next interval start. */
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

	def byLinearIntervalIndex: immutable.IndexedSeq[LinearInterval[A]]
}