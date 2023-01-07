package wms.flow.planner
package engine

import global.*
import graph.*
import math.*
import queue.{*, given}
import resource.*


object PowerPlanOptimizer {

	trait PowerPlan {
		def costAt(pieceIndex: PieceIndex, stage: Stage): Money

		def withPowerIncreasedAt(pieceIndex: PieceIndex, stage: Stage): List[PowerPlan]

		def withPowerDecreasedAt(pieceIndex: PieceIndex, stage: Stage): List[PowerPlan]
	}
}

class PowerPlanOptimizer[PA <: PiecewiseAlgebra, CG <: ClosedGraph](val piecewiseAlgebra: PA, val closedGraph: CG) {
	import PowerPlanOptimizer.*
	import closedGraph.*
	import piecewiseAlgebra.*

	def optimize(initialInputQueue: Mapping[Queue], upstreamTrajectoryBySource: SourceN[?] => Trajectory[Queue], initialPlan: PowerPlan)(progressTraveler: PowerPlan => Boolean): Unit = {

//		buildLazyTrajectory()


		???
	}

}
