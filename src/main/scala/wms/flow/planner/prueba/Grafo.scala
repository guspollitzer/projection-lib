package wms.flow.planner
package prueba

import engine.RequiredPowerCalculator
import graph.*
import math.*
import queue.{FifoQueue, PriorityQueue, Heap, total}
import StaggeredTrajectoryAlgebra.*
import time.*
import time.Instant.{*, given}
import global.*


import scala.collection.immutable.TreeMap
import scala.language.implicitConversions

object Grafo {

	def main(args: Array[String]): Unit = {

		val heap: Heap = Map.empty[Category, Quantity]
		val load = heap.total

		val eClosedGraph = ClosedGraph.build(
			builder => {
				given ClosedGraph.Builder = builder

				val source = Source("source")
				val fork = Fork2[PriorityQueue, FifoQueue]("fork")
				val flow = Flow[FifoQueue, FifoQueue]("flow")
				val join = Join2[FifoQueue, PriorityQueue]("join")
				val sink = Sink("sink")

				source.out ~> fork.in
				fork.outA ~> flow.in
				flow.out ~> join.inA
				fork.outB ~> join.inB
				join.out ~> sink.in
			}
		)

		println(eClosedGraph)

//		eClosedGraph.map {
//			closedGraph =>
//
//				val steps = IndexedSeq(Step(1f, 2f, TreeMap.empty[Priority, Heap]))
//				val sinkDownstreamDemandAlgebra = new StaggeredTrajectoryAlgebra(steps.view)
//				val rpc = new RequiredPowerCalculator(sinkDownstreamDemandAlgebra)
//
//				val stateAtStartingInstant: GraphMap[rpc.StageState] = GraphMap
//					.fill[rpc.StageState](closedGraph)(stage => stage.name match {
//						case "flow" | "join" =>
//							val pt = rpc.buildSinkDemandQueueTrajectory(
//
//						case _ => rpc.StageState(Left(TreeMap.empty))
//					}
//					)
//
//				val desiredBacklogAtEndingInstant: GraphMap[Duration] = GraphMap.fill(closedGraph)(stage => ??? )
//
//				rpc.calcRequiredPowerAt(
//					0f,
//					stateAtStartingInstant,
//					3f,
//					desiredBacklogAtEndingInstant,
//					Map(Path.A -> closedGraph.getSinks(0))
//				)
//		}
	}
}
