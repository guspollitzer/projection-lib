package wms.flow.planner
package engine

import graph.*
import math.*
import global.*
import time.*
import queue.{*, given}

object Simulator {
	case class Log(processed: Queue, backlogAtEndingInstant: Queue, shortage: Quantity)
}

class Simulator(val piecewiseAlgebra: PiecewiseAlgebra) {

	import piecewiseAlgebra.*
	import Simulator.*

	def simulate(
		initialBacklog: GraphMap[Queue],
		upstreamTrajectoryBySource: Map[Source[?], Trajectory[Queue]],
		powerTrajectory: Trajectory[GraphMap[Quantity]],
		//		sourceByPath: Map[Path, Source[?]]
	): Trajectory[GraphMap[Log]] = {

		piecewiseAlgebra.buildTrajectory[GraphMap[Queue], GraphMap[Log]](initialBacklog) {
			(backlogAtStart: GraphMap[Queue], pieceIndex: Int, start: Instant, end: Instant) =>

				backlogAtStart.calcDownward[Log] { (stage: Stage, backlogAtStartAtStage: Queue, alreadyCalculatedLogs: Map[Stage, Log]) =>
					val upstreamAtStage = getUpstreamPush(stage, alreadyCalculatedLogs) { source => upstreamTrajectoryBySource(source).getWholePieceIntegralAt(pieceIndex) }
					val source: Queue = backlogAtStartAtStage ++ upstreamAtStage
					val consumption: Consumption[Queue] = source.consumed(powerTrajectory.getWholePieceIntegralAt(pieceIndex).get(stage))
					Log(consumption.consumed, consumption.remaining, consumption.shortage)
				}
		} {
			gm => gm.map { (stage, log) => log.backlogAtEndingInstant }
		}
	}

	private def getUpstreamPush(
		stage: Stage,
		alreadyCalculatedLogs: Map[Stage, Log],
	)(sourcePushGetter: Source[?] => Queue): Queue = {
		stage match {
			case source: Source[?] => sourcePushGetter(source)

			case sink: Sink[?] => alreadyCalculatedLogs(sink.in.from.host).processed

			case flow: Flow[?, ?] => alreadyCalculatedLogs(flow.in.from.host).processed

			case nToM: NToM[?, ?] =>
				val logs: IndexedSeq[Option[Log]] = nToM.ins.map(in => alreadyCalculatedLogs.get(in.from.host));
				val queues = for oLog <- logs; log <- oLog yield log.processed
				queues.reduce { (inAPush, inBPush) => inAPush.mergedWith(inBPush) }
		}
	}
}
