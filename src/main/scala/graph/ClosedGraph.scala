package wms.flow.planner
package graph

import collection.IterableOnce

object ClosedGraph {

	def build(sketchs: IterableOnce[StageSketch])(builder: Builder => Unit): ClosedGraph = {
		builder.apply(Builder())

		val seqBuilder = IndexedSeq.newBuilder[Stage]
		var index = 0;
		for sketch <- sketchs do {
			seqBuilder += sketch.arise(index)
			index += 1
		}
		ClosedGraph(seqBuilder.result())
	}

	class Builder private[ClosedGraph] {
	}

}

class ClosedGraph private[graph](val stages: IndexedSeq[Stage])
