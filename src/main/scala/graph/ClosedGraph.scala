package wms.flow.planner
package graph

import collection.IterableOnce
import collection.mutable.ArrayBuffer

object ClosedGraph {

	def build(body: Builder => Unit): ClosedGraph = {
		val builder = new Builder()
		body.apply(builder)

		val seqBuilder = IndexedSeq.newBuilder[Stage]
		var index = 0;
		for sketch <- builder.sketchs do {
			seqBuilder += sketch.arise(index)
			index += 1
		}
		ClosedGraph(seqBuilder.result())
	}

	class Builder private[ClosedGraph]() {
		val sketchs = ArrayBuffer[Sketch]()

		def register(sketch: Sketch): Unit = {
			if !sketchs.contains(sketch) then
				sketchs.addOne(sketch)
		}
	}

}

class ClosedGraph private[ClosedGraph](val stages: IndexedSeq[Stage])
