package wms.flow.planner
package engine

import graph.*
import math.*
import queue.{*, given}
import resource.*


object PowerPlanOptimizer {

	trait PowerPlan {
		def costAt(pieceIndex: Int, stage: Stage): Money

		def withPowerIncreasedAt(pieceIndex: Int, stage: Stage): List[PowerPlan]

		def withPowerDecreasedAt(pieceIndex: Int, stage: Stage): List[PowerPlan]
	}
}

class PowerPlanOptimizer[PA <: PiecewiseAlgebra, CG <: ClosedGraph](val piecewiseAlgebra: PA, val closedGraph: CG) {
	import PowerPlanOptimizer.*
	import closedGraph.*
	import piecewiseAlgebra.*

	def optimize(initialBacklog: Mapping[Queue], upstreamTrajectoryBySource: SourceN[?] => Trajectory[Queue], initialPlan: PowerPlan)(progressTraveler: PowerPlan => Boolean): Unit = {

//		buildLazyTrajectory()


		???
	}

}
