package wms.flow.planner
package engine

import math.PiecewiseAlgebra
import queue.{*, given}
import graph.*
import workflow.*
import global.*

class UpstreamTools[PA <: PiecewiseAlgebra](val piecewiseAlgebra: PA) {
	import piecewiseAlgebra.*

	def buildTrajectoryBySourceGetter(
		upstreamTrajectory: Trajectory[Queue],
		sourceByPath: Map[Path, Source[?]]
	): SourceN[?] => Trajectory[Queue] = {

		def upstreamTrajectoryFor(source: SourceN[?]): Trajectory[Queue] = {
			for queue <- upstreamTrajectory yield {
				queue.filterByCategory {
					category => sourceByPath.get(category.path).contains(source)
				}
			}
		}

		upstreamTrajectoryFor
	}

}
