package wms.flow.planner
package engine

import global.*
import graph.*
import queue.*
import resource.*
import time.*
import workflow.*

object PieceCostCalculator {
	case class StageInfo(backlogAtPieceStart: Queue, processedDuringPiece: Queue, backlogAtPieceEnd: Queue, backlogShortageDuringPiece: Quantity, desiredBacklogAtEndingInstant: DesiredBacklog)	
}

class PieceCostCalculator[CG <: ClosedGraph](val closedGraph: CG) {
	import PieceCostCalculator.*
	import closedGraph.*

	def calc(
		trajectoryStart: Instant,
		pieceStart: Instant,
		pieceEnd: Instant,
		graphInfo: Mapping[StageInfo],
	): Money = {
		
		???
	}

}
