package wms.flow.planner
package prueba

import engine.RequiredPowerCalculator
import RequiredPowerCalculator.*
import graph.*
import math.*
import queue.{*, given}
import StaggeredAlgebra.*
import time.{*, given}
import time.Instant.{*, given}
import global.*
import util.*
import workflow.*
import resource.*

import scala.annotation.targetName
import scala.collection.immutable.TreeMap
import scala.language.implicitConversions

object Grafo {

	def main(args: Array[String]): Unit = {

		val heap: Heap = Heap(Map.empty)
		val load = heap.total

		val eClosedGraph = ClosedGraph.build(
			builder => {
				given ClosedGraph.Builder = builder

				val source = Source[PriorityQueue]("source")
				val fork = NToM[PriorityQueue, FifoQueue]("fork", 1, 2)
				val flow = Flow[FifoQueue, FifoQueue]("flow")
				val join = NToM[FifoQueue, PriorityQueue]("join", 2, 1)
				val sink = Sink[PriorityQueue]("sink")

				source.out ~> fork.ins(0)
				fork.outs(0) ~> flow.in
				flow.out ~> join.ins(0)
				fork.outs(1) ~> join.ins(1)
				join.outs(0) ~> sink.in
			}
		)

		println(eClosedGraph)

		val quantityFractionable: Fractionable[Quantity] = new Fractionable[Quantity] {
			extension (a: Quantity) def takeFraction(fraction: Quantity): Quantity = a * fraction
		}

		val quantityConcatenable: Concatenable[Quantity] = new Concatenable[Quantity] {
			override def empty = 0

			extension (a: Quantity) {
				@targetName("concat")
				def ++(b: Quantity): Quantity = a + b
			}
		}

		val facForQuantity = FractionAndConcatOpsFor[Quantity](quantityFractionable, quantityConcatenable)

		given opsSummoner: FractionAndConcatOpsSummoner = new FractionAndConcatOpsSummoner(facForQuantity)

		eClosedGraph.map {
			closedGraph =>

				val pieceEndingInstantByIndex = IArray.tabulate[Instant](3)(i => i + 1f);
				val algebra = new StaggeredAlgebra(0f, pieceEndingInstantByIndex);
				val rpc = new RequiredPowerCalculator[algebra.type, closedGraph.type](algebra, closedGraph);

				val stateAtStartingInstant: closedGraph.Mapping[Queue] = closedGraph
					.createMapping[Queue](stage => CaseA(PriorityQueue.from(TreeMap.empty[Priority, Heap])))

				val desiredBacklogAtEndingInstant: closedGraph.Mapping[algebra.Trajectory[DesiredBacklog]] =
					closedGraph.createMapping(stage => algebra.buildTrajectory(pieceIndex => stage match {
						case source: Source[?] => Maximal
						case _ => Minimal(10f)
					}))

				val backlogCapacity = closedGraph.createMapping(stage => 10)
				val theSink = closedGraph.getSinks(0)
				val sinkByPath = Map(Path.A -> theSink, Path.B -> theSink, Path.C -> theSink)

				val downstreamDemandTrajectory = algebra.buildTrajectory(pieceIndex => PriorityQueue.from(TreeMap.empty[Priority, Heap]))
				val x = rpc.calcRequiredPowerTrajectory(
					stateAtStartingInstant,
					desiredBacklogAtEndingInstant,
					backlogCapacity,
					sinkByPath,
					downstreamDemandTrajectory
				)
				x
		}
	}

	def borrame(): Unit = {
		import engine.*

		val eClosedGraph = ClosedGraph.build(
			builder => {
				given ClosedGraph.Builder = builder

				val source = Source[PriorityQueue]("source")
				val fork = NToM[PriorityQueue, FifoQueue]("fork", 1, 2)
				val flow = Flow[FifoQueue, FifoQueue]("flow")
				val join = NToM[FifoQueue, PriorityQueue]("join", 2, 1)
				val sink = Sink[PriorityQueue]("sink")

				source.out ~> fork.ins(0)
				fork.outs(0) ~> flow.in
				flow.out ~> join.ins(0)
				fork.outs(1) ~> join.ins(1)
				join.outs(0) ~> sink.in
			}
		)

		val quantityFractionable: Fractionable[Quantity] = new Fractionable[Quantity] {
			extension (a: Quantity) def takeFraction(fraction: Quantity): Quantity = a * fraction
		}

		val quantityConcatenable: Concatenable[Quantity] = new Concatenable[Quantity] {
			override def empty = 0

			extension (a: Quantity) {
				@targetName("concat")
				def ++(b: Quantity): Quantity = a + b
			}
		}

		val facForQuantity = FractionAndConcatOpsFor[Quantity](quantityFractionable, quantityConcatenable)

		given opsSummoner: FractionAndConcatOpsSummoner = new FractionAndConcatOpsSummoner(facForQuantity)

		eClosedGraph.map {
			closedGraph =>
				import closedGraph.*

				val pieceEndingInstantByIndex = IArray.tabulate[Instant](3)(i => i + 1f);
				val algebra = new StaggeredAlgebra(0f, pieceEndingInstantByIndex);
				import algebra.*

				val pcc = new PlanCostCalculator[algebra.type, closedGraph.type](algebra, closedGraph)(Map.empty);
				val initialInputQueue: Mapping[Queue] = createMapping(stage => CaseB(List.empty))
				val upstreamTrajectoryBySource: SourceN[?] => Trajectory[Queue] = source => algebra.buildTrajectory[Queue]( (index: PieceIndex) => CaseB(List.empty) );
				val powerPlan = new PlanCostCalculator.PowerPlan[closedGraph.type](closedGraph) {
					def getPowerAt(pieceIndex: PieceIndex): Mapping[Quantity] = ???

					def getCostAt(pieceIndex: PieceIndex): Mapping[Money] = ???
				}

				pcc.calc(initialInputQueue, upstreamTrajectoryBySource, powerPlan);
		}
	}
}
