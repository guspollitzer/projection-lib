package wms.flow.planner
package graph

case class GraphMap[A](closedGraph: ClosedGraph, values: IndexedSeq[A]) {
	assert(values.length == closedGraph.stages.length)

	def get(stage: Stage): A = values.apply(stage.ordinal)

}
