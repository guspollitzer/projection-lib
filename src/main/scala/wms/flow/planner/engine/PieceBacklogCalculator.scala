package wms.flow.planner
package engine

import global.*
import graph.*
import queue.{*, given}
import time.*
import workflow.*

object PieceBacklogCalculator {
	case class Log(processed: Queue, backlogAtEnd: Queue, shortage: Quantity)
}

class PieceBacklogCalculator[CG <: ClosedGraph](val closedGraph: CG) {
	import PieceBacklogCalculator.*
	import closedGraph.*

	def calc(backlogAtStart: Mapping[Queue], power: Mapping[Quantity])(upstreamBySource: SourceN[?] => Queue): Mapping[Log] = {

		def getUpstreamPush(
			stage: Stage,
			alreadyCalculatedLogs: Map[Stage, Log],
		): Queue = {
			stage match {
				case source: SourceN[?] => upstreamBySource(source)

				case join: Join[?] =>
					val logs: IndexedSeq[Option[Log]] = join.ins.map(in => alreadyCalculatedLogs.get(in.from.host));
					val queues = for oLog <- logs; log <- oLog yield log.processed
					queues.reduce { (inAPush, inBPush) => inAPush.mergedWith(inBPush) }
			}
		}

		backlogAtStart.calcDownward[Log] {
			(stage: Stage, backlogAtStartAtStage: Queue, alreadyCalculatedLogs: Map[Stage, Log]) =>
				val upstreamAtStage = getUpstreamPush(stage, alreadyCalculatedLogs)
				val source: Queue = backlogAtStartAtStage ++ upstreamAtStage
				val consumption: Consumption[Queue] = source.consumed(power.get(stage))
				Log(consumption.consumed, consumption.remaining, consumption.shortage)
		}
	}
}
