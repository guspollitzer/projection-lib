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

			val waving = Source("waving")
			val buffering = Source("buffering")
			val picking = NToM[PriorityQueue, PriorityQueue]("picking", 2, 2)
			val sorting = Flow[PriorityQueue, PriorityQueue]("sorting")
			val walling = Flow[PriorityQueue, PriorityQueue]("walling")
			val packingNormal = Sink("packingNormal")
			val packingWall = Sink("packingWall")

			waving.out ~> picking.ins(0)
			buffering.out ~> picking.ins(1)
			picking.outs(0) ~> packingNormal.in
			picking.outs(1) ~> sorting.in
			sorting.out ~> walling.in
			walling.out ~> packingWall.in
		}
	)


}
