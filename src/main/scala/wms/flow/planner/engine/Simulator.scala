package wms.flow.planner
package engine

import graph.*
import math.*
import global.*
import time.*
import queue.{*, given}
import util.*

class Simulator[PA <: PiecewiseAlgebra, CG <: ClosedGraph](val piecewiseAlgebra: PA, val closedGraph: CG) {
	import piecewiseAlgebra.*
	import closedGraph.*

	private val flowProjectionPieceCalculator = new FlowProjectionPieceCalculator[closedGraph.type](closedGraph)

	@deprecated
	def simulate(
		initialInputQueue: Mapping[Queue],
		upstreamTrajectoryBySource: SourceN[?] => Trajectory[Queue],
		powerTrajectory: Trajectory[Mapping[Quantity]],
	): Trajectory[Mapping[FlowProjectionPieceCalculator.StageProjection]] = {

		piecewiseAlgebra.buildTrajectory[Mapping[Queue], Mapping[FlowProjectionPieceCalculator.StageProjection]](initialInputQueue) {
			(inputQueueAtStart: Mapping[Queue], pieceIndex: PieceIndex, start: Instant, end: Instant) =>

				val power = powerTrajectory.getValueAt(pieceIndex);

				flowProjectionPieceCalculator.calc(pieceIndex, inputQueueAtStart, power)(source => upstreamTrajectoryBySource(source).getValueAt(pieceIndex))
		} {
			(_, logMapping) => logMapping.map(_.inputQueueAtEnd)
		}
	}

}
