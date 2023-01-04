package wms.flow.planner
package engine

import global.*
import graph.*
import queue.{*, given}
import time.*
import workflow.*
import math.PiecewiseAlgebra

object PieceBacklogCalculator {
	/** The calculated backlog at a [[Stage]] at the end of a [[PiecewiseAlgebra.Trajectory]]'s piece. */
	case class CalculatedBacklog(processed: Queue, backlogAtEnd: Queue, shortage: Quantity)
}

class PieceBacklogCalculator[CG <: ClosedGraph](val closedGraph: CG) {
	import PieceBacklogCalculator.*
	import closedGraph.*

	def calc(backlogAtStart: Mapping[Queue], power: Mapping[Quantity])(upstreamBySource: SourceN[?] => Queue): Mapping[CalculatedBacklog] = {

		def getUpstreamPush(
			stage: Stage,
			alreadyCalculatedLogs: Map[Stage, CalculatedBacklog],
		): Queue = {
			stage match {
				case source: SourceN[?] => upstreamBySource(source)

				case join: Join[?] =>
					val logs: IndexedSeq[Option[CalculatedBacklog]] = join.ins.map(in => alreadyCalculatedLogs.get(in.from.host));
					val queues = for oLog <- logs; log <- oLog yield log.processed
					queues.reduce { (inAPush, inBPush) => inAPush.mergedWith(inBPush) }
			}
		}

		backlogAtStart.calcDownward[CalculatedBacklog] {
			(stage: Stage, backlogAtStartAtStage: Queue, alreadyCalculatedLogs: Map[Stage, CalculatedBacklog]) =>
				val upstreamAtStage = getUpstreamPush(stage, alreadyCalculatedLogs)
				val source: Queue = backlogAtStartAtStage ++ upstreamAtStage
				val consumption: Consumption[Queue] = source.consumed(power(stage))
				CalculatedBacklog(consumption.consumed, consumption.remaining, consumption.shortage)
		}
	}
}
