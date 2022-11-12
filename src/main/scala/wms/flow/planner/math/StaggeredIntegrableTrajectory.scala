package wms.flow.planner
package math

import time.*

import PiecewiseIntegrableTrajectory.Piece
import StaggeredIntegrableTrajectory.Step

import scala.collection.immutable

object StaggeredIntegrableTrajectory {

	case class Step[A: Fractionable](start: Instant, end: Instant, wholeIntegral: A) extends Piece[A] {

		override def integral(from: Instant, to: Instant): A =
			assert(from >= start && to <= end)

			wholeIntegral.fractionate((to - from) / (end - start))
	}

}

case class StaggeredIntegrableTrajectory[A : Fractionable](
	byPieceIndex: immutable.IndexedSeq[Step[A]]
) extends PiecewiseIntegrableTrajectory[A] {



//	override def byLinearIntervalIndex: immutable.IndexedSeq[StaggedInterval[A]] = steps
}
