package wms.flow.planner
package engine

import global.*
import graph.*
import queue.{*, given}
import time.*
import workflow.*
import math.PiecewiseAlgebra

object FlowProjectionPieceCalculator {
	/** The log of what will happen at a [[Stage]] during a piece of a [[PiecewiseAlgebra.Trajectory]], according to this projection calculator.
	  * @param processed the amount of elements that will be processed during the piece, grouped by category in processing order.
	  * @param inputQueueAtEnd the queue of elements (grouped by category) that will be waiting in the input of the stage at the end of the piece.
	  * @param shortage the number of items that could have been processed but won't because the input queue will be empty.. */
	case class StageProjection(processed: Queue, inputQueueAtEnd: Queue, shortage: Quantity)
}

class FlowProjectionPieceCalculator[CG <: ClosedGraph](val closedGraph: CG) {
	import FlowProjectionPieceCalculator.*
	import closedGraph.*

	def calc(inputQueueAtStart: Mapping[Queue], power: Mapping[Quantity])(upstreamBySource: SourceN[?] => Queue): Mapping[StageProjection] = {

		def getUpstreamPush(
			stage: Stage,
			alreadyCalculatedLogs: Map[Stage, StageProjection],
		): Queue = {
			stage match {
				case source: SourceN[?] => upstreamBySource(source)

				case join: Join[?] =>
					val logs: IndexedSeq[Option[StageProjection]] = join.ins.map(in => alreadyCalculatedLogs.get(in.from.host));
					val queues = for oLog <- logs; log <- oLog yield log.processed
					queues.reduce { (inAPush, inBPush) => inAPush.mergedWith(inBPush) }
			}
		}

		inputQueueAtStart.calcDownward[StageProjection] {
			(stage: Stage, inputQueueAtStartAtStage: Queue, alreadyCalculatedLogs: Map[Stage, StageProjection]) =>
				val upstreamAtStage = getUpstreamPush(stage, alreadyCalculatedLogs)
				val source: Queue = inputQueueAtStartAtStage ++ upstreamAtStage
				val consumption: Consumption[Queue] = source.consumed(power(stage))
				StageProjection(consumption.consumed, consumption.remaining, consumption.shortage)
		}
	}
}
