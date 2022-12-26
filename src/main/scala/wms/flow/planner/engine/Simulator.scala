package wms.flow.planner
package engine

import graph.*
import math.*
import global.*
import time.*
import queue.{*, given}
import wms.flow.planner.util.CaseA

object Simulator {
	case class Log(processed: Queue, backlogAtEndingInstant: Queue, shortage: Quantity)
}

class Simulator(val piecewiseAlgebra: PiecewiseAlgebra) {

	import piecewiseAlgebra.*
	import Simulator.*

	def simulate(
		initialBacklog: GraphMap[Queue],
		upstreamTrajectory: Trajectory[Queue],
		powerTrajectory: Trajectory[GraphMap[Quantity]],
		sourceByPath: Map[Path, Source[?]]
	): Trajectory[GraphMap[Log]] = {

		def upstreamTrajectoryFor(source: SourceN[?]): Trajectory[Queue] = {
			for queue <- upstreamTrajectory yield {
				queue.filterByCategory {
					category => sourceByPath.get(category.path).contains(source)
				}
			}
		}
		simulate(initialBacklog, upstreamTrajectoryFor, powerTrajectory);
	}

	def simulate(
		initialBacklog: GraphMap[Queue],
		upstreamTrajectoryBySource: SourceN[?] => Trajectory[Queue],
		powerTrajectory: Trajectory[GraphMap[Quantity]],
	): Trajectory[GraphMap[Log]] = {

		piecewiseAlgebra.buildTrajectory[GraphMap[Queue], GraphMap[Log]](initialBacklog) {
			(backlogAtStart: GraphMap[Queue], pieceIndex: Int, start: Instant, end: Instant) =>

				def getUpstreamPush(
					stage: Stage,
					alreadyCalculatedLogs: Map[Stage, Log],
				): Queue = {
					stage match {
						case source: SourceN[?] => upstreamTrajectoryBySource(source).getWholePieceIntegralAt(pieceIndex)

						case join: Join[?] =>
							val logs: IndexedSeq[Option[Log]] = join.ins.map(in => alreadyCalculatedLogs.get(in.from.host));
							val queues = for oLog <- logs; log <- oLog yield log.processed
							queues.reduce { (inAPush, inBPush) => inAPush.mergedWith(inBPush) }
					}
				}

				backlogAtStart.calcDownward[Log] { (stage: Stage, backlogAtStartAtStage: Queue, alreadyCalculatedLogs: Map[Stage, Log]) =>
					val upstreamAtStage = getUpstreamPush(stage, alreadyCalculatedLogs)
					val source: Queue = backlogAtStartAtStage ++ upstreamAtStage
					val consumption: Consumption[Queue] = source.consumed(powerTrajectory.getWholePieceIntegralAt(pieceIndex).get(stage))
					Log(consumption.consumed, consumption.remaining, consumption.shortage)
				}
		} {
			gm => gm.map { (stage, log) => log.backlogAtEndingInstant }
		}
	}

}
