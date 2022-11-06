package wms.flow.planner
package prueba

import graph.*

object Grafo {

	def main(args: Array[String]): Unit = {

		val result = ClosedGraph.build(builder => {
			given ClosedGraph.Builder = builder

			val source = Source("source")
			val sink = Sink("sink")

			source.out ~> sink.in
		})

		println(result)

	}
}
