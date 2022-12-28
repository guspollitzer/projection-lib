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

	private val pieceBacklogCalculator = new PieceBacklogCalculator[closedGraph.type](closedGraph)

	@deprecated
	def simulate(
		initialBacklog: Mapping[Queue],
		upstreamTrajectoryBySource: SourceN[?] => Trajectory[Queue],
		powerTrajectory: Trajectory[Mapping[Quantity]],
	): Trajectory[Mapping[PieceBacklogCalculator.Log]] = {

		piecewiseAlgebra.buildTrajectory[Mapping[Queue], Mapping[PieceBacklogCalculator.Log]](initialBacklog) {
			(backlogAtStart: Mapping[Queue], pieceIndex: Int, start: Instant, end: Instant) =>

				val power = powerTrajectory.getWholePieceIntegralAt(pieceIndex);

				pieceBacklogCalculator.calc(backlogAtStart, power)(source => upstreamTrajectoryBySource(source).getWholePieceIntegralAt(pieceIndex))
		} {
			logMapping => logMapping.mapValues(_.backlogAtEnd)
		}
	}

}
