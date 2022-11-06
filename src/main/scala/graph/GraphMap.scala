package wms.flow.planner
package graph

case class GraphMap[+A](closedGraph: ClosedGraph, values: IndexedSeq[A]) {
	assert(values.size == closedGraph.stages.size)

	def get(stage: Stage): A = values.apply(stage.ordinal)

	def map[B](f: A => B): GraphMap[B] = GraphMap(closedGraph, values.map(f))
}

object GraphMap {
	def fill[A](graph: ClosedGraph)(f: Stage => A): GraphMap[A] = new GraphMap[A](graph, graph.stages.map(f))
}
