package wms.flow.planner
package engine

import global.*
import graph.{ClosedGraph, GraphMap, Stage}
import math.PiecewiseIntegrableTrajectory
import queue.{Heap, PriorityQueue}
import time.Instant

import scala.annotation.tailrec
import scala.collection.immutable


class RequiredPowerCalculator(graph: ClosedGraph) {

	
	def calc(forecast: PiecewiseIntegrableTrajectory[Heap]): RequiredPower = ???

	case class RequiredPower()

	case class StageState(pepe: Int)
	trait StageProcessor {
	}

	def calcRequiredPowerAt(
		startingInstant: Instant,
		stateAtStartingInstant: GraphMap[StageState],
		endingInstant: Instant,
		downstreamDemand: PriorityQueue
	): GraphMap[StageState] = {

		???
	}

}
