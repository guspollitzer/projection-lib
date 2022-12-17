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

			val waving = Source[PriorityQueue]("waving")
			val buffering = Source[PriorityQueue]("buffering")
			val picking = NToM[PriorityQueue, PriorityQueue]("picking", 2, 2)
			val sorting = Flow[PriorityQueue, PriorityQueue]("sorting")
			val walling = Flow[PriorityQueue, PriorityQueue]("walling")
			val packingNormal = Sink[PriorityQueue]("packingNormal")
			val packingWall = Sink[PriorityQueue]("packingWall")

			waving.out ~> picking.ins(0)
			buffering.out ~> picking.ins(1)
			picking.outs(0) ~> packingNormal.in
			picking.outs(1) ~> sorting.in
			sorting.out ~> walling.in
			walling.out ~> packingWall.in
		}
	)


	private val eInboundClosedGraph = ClosedGraph.build(
		builder => {
			given ClosedGraph.Builder = builder

			val normalReceiving = Source[FifoQueue]("normal receiving")
			val directReceiving = Source[FifoQueue]( "direct receiving")
			val checkIn = Flow[FifoQueue, FifoQueue]("check-in")
			val putAway = Join2[FifoQueue, FifoQueue]("putAway")
			val stock = Sink[FifoQueue]("stock")

			normalReceiving.out ~> checkIn.in
			checkIn.out ~> putAway.inA
			directReceiving.out ~> putAway.inB
			putAway.out ~> stock.in
		}
	)


}
