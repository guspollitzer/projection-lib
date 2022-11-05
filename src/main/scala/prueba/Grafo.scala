package wms.flow.planner
package prueba

import graph.*
import sketchs.*

class Grafo {

	ClosedGraph.build(builder => {
		given ClosedGraph.Builder = builder

		val source = Source("source")
		val sink = Sink("sink")

		source.outPort ~> sink.inPort
	})
}
