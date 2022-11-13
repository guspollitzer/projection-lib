package wms.flow.planner
package prueba

import graph.*
import queue.{PriorityQueue, FifoQueue}

object Grafo {

	def main(args: Array[String]): Unit = {

		val result = ClosedGraph.build(
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

		println(result)

	}
}
