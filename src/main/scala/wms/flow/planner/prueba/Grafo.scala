package wms.flow.planner
package prueba

import engine.RequiredPowerCalculator
import graph.*
import math.*
import queue.{total, Concatenable, FifoQueue, Heap, PriorityQueue, given}
import StaggeredAlgebra.*
import time.{*, given}
import time.Instant.{*, given}
import global.*
import util.*

import scala.annotation.targetName
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

		given Fractionable[Quantity] with {
			extension (a: Quantity) def takeFraction(fraction: Quantity): Quantity = a * fraction
		}
		given Concatenable[Quantity] with {
			def empty = 0

			extension (a: Quantity) {
				@targetName("concat")
				def ++(b: Quantity) = a + b
			}
		}

		val facForQuantity = FractionAndConcatOpsFor[Quantity](summon[Fractionable[Quantity]], summon[Concatenable[Quantity]])

		given pepe: FractionAndConcatOpsSummoner = new FractionAndConcatOpsSummoner(facForQuantity)

		eClosedGraph.map {
			closedGraph =>

				val pieceEndingInstantByIndex = IArray.tabulate[Instant](3)(i => i + 1f);
				val algebra = new StaggeredAlgebra(0f, pieceEndingInstantByIndex);
				val rpc = new RequiredPowerCalculator(algebra);

				val stateAtStartingInstant: GraphMap[rpc.SIS] = GraphMap
					.fill[rpc.SIS](closedGraph)(stage => CaseA(rpc.StageInitialState(PriorityQueue.from(TreeMap.empty[Priority, Heap]))))

				val desiredBacklogAtEndingInstant: GraphMap[rpc.piecewiseAlgebra.Trajectory[Duration]] =
					GraphMap.fill(closedGraph)(stage => rpc.piecewiseAlgebra.buildTrajectory(pieceIndex => 10f))

				val maxBacklogLoad = GraphMap.fill(closedGraph)(stage => 10)
				val theSink = closedGraph.getSinks(0)
				val sinkByPath = Map(Path.A -> theSink, Path.B -> theSink, Path.C -> theSink)

				val downstreamDemandTrajectory = rpc.piecewiseAlgebra.buildTrajectory(pieceIndex => PriorityQueue.from(TreeMap.empty[Priority, Heap]))
				val x = rpc.calcRequiredPowerTrajectory(
					stateAtStartingInstant,
					desiredBacklogAtEndingInstant,
					maxBacklogLoad,
					sinkByPath,
					downstreamDemandTrajectory
				)
				x
		}
	}
}
