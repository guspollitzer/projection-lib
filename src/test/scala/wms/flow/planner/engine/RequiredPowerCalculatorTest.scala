package wms.flow.planner
package engine

import org.scalacheck._
import Arbitrary._
import Prop._

import graph.*
import queue.*


object RequiredPowerCalculatorTest extends Properties("simple") {

	private val eClosedGraph = ClosedGraph.build(
		builder => {
			given ClosedGraph.Builder = builder

			val wavingA = Source("wavingA")
			val wavingB = Source("wavingB")
			val wavesJoiner = Join2[PriorityQueue, PriorityQueue]("joiner")
			val picking = Fork2[PriorityQueue, PriorityQueue]("picking")
			val sorting = Flow[PriorityQueue, PriorityQueue]("sorting")
			val packingNormal = Sink("packingNormal")
			val packingWall = Sink("packingWall")

			wavingA.out ~> wavesJoiner.inA
			wavingB.out ~> wavesJoiner.inB
			wavesJoiner.out ~> picking.in
			picking.outA ~> packingNormal.in
			picking.outB ~> sorting.in
			sorting.out ~> packingWall.in
		}
	)


}
