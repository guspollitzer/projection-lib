package wms.flow.planner
package engine

import scala.collection.immutable

import global.*
import graph.{ClosedGraph, GraphMap}
import math.PiecewiseIntegrableTrajectory
import queue.{Heap, PriorityQueue}
import time.Instant


class RequieredPowerCalculator(graph: ClosedGraph) {

	
	def calc(forecast: PiecewiseIntegrableTrajectory[Heap]): RequiredPower = ???

	case class RequiredPower()

	case class StageState()

	trait StageProcessor {
	}

	def calcRequieredPowerAt(
		stargingInstant: Instant,
		stateAtStartingInstant: GraphMap[StageState],
		endingInstant: Instant,
		downstreamDemand: PriorityQueue
	): GraphMap[StageState] = {
		val graphMap = GraphMap.fill[Unit](graph)(stage => ())

		???
	}

}
