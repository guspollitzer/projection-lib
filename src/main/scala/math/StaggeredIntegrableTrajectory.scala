package wms.flow.planner
package math

import time.*

import PiecewiseIntegrableTrajectory.LinearInterval
import StaggeredIntegrableTrajectory.Interval

import scala.collection.immutable

object StaggeredIntegrableTrajectory {

	case class Interval[A: Fractionable](start: Instant, end: Instant, wholeIntegral: A) extends LinearInterval[A] {

		override def integral(from: Instant, to: Instant): A =
			assert(from >= start && to <= end)

			wholeIntegral.fractionate((to - from) / (end - start))
	}

}

case class StaggeredIntegrableTrajectory[A : Fractionable](
	byLinearIntervalIndex: immutable.IndexedSeq[Interval[A]]
) extends PiecewiseIntegrableTrajectory[A] {



//	override def byLinearIntervalIndex: immutable.IndexedSeq[StaggedInterval[A]] = steps
}
