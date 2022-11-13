package wms.flow.planner
package graph

import scala.annotation.tailrec

case class GraphMap[+A](closedGraph: ClosedGraph, values: IndexedSeq[A]) {
	assert(values.size == closedGraph.stages.size)

	def get(stage: Stage): A = values.apply(stage.ordinal)

	def map[B](f: (Stage, A) => B): GraphMap[B] = {
		val mappedValues = for index <- closedGraph.stages.indices yield f(closedGraph.stages(index), values(index))
		GraphMap(closedGraph, mappedValues)
	}

	case class ValueAtStage[T](stage: Stage, value: T)

	/** Creates a new [[GraphMap]] whose values are calculated applying the function `f` on each value of this instance,
	 * starting with the [[Sink]] stages and continuing with the stages that feed the already calculated stages. */
	def calcUpward[B](f: (stage: Stage, oldValue: A, alreadyCalculated: Map[Stage, B]) => B): GraphMap[B] = {
		calcDependentwardly(s => s.downstreamStages)(f)
	}

	/** Creates a new [[GraphMap]] whose values are calculated applying the function `f` on each value of this instance,
	 * starting with the [[Source]] stages and continuing with the stages fed by the already calculated stages. */
	def calcDownward[B](f: (stage: Stage, value: A, alreadyCalculated: Map[Stage, B]) => B): GraphMap[B] = {
		calcDependentwardly(s => s.upstreamStages)(f)
	}


	/** Creates a new [[GraphMap]] whose values are calculated applying the function `f` on each value of this instance,
	 * starting with the independent stages and continuing with the stages that depend on the already calculated stages.
	 * The dependency relationship between stages is specified by the `dependencyGetter` function. */
	def calcDependentwardly[B](dependenciesGetter: Stage => Set[Stage])
		(f: (stage: Stage, oldValue: A, alreadyCalculated: Map[Stage, B]) => B)
	: GraphMap[B] = {

		@tailrec
		def loop(
			notTried: List[ValueAtStage[A]],
			tried: List[ValueAtStage[A]],
			alreadyCalculated: Map[Stage, B]
		): Map[Stage, B] = {
			notTried match {
				case Nil =>
					if tried.isEmpty
					then alreadyCalculated
					else loop(tried, Nil, alreadyCalculated)

				case head :: tail =>
					val dependencies = dependenciesGetter(head.stage)
					if dependencies.exists(stage => !alreadyCalculated.contains(stage))
					then loop(tail, head :: tried, alreadyCalculated)
					else {
						val b = f(
							head.stage,
							head.value,
							alreadyCalculated
						)
						loop(tail, tried, alreadyCalculated + (head.stage -> b))
					}
			}
		}

		val stateBySink = for sink <- closedGraph.getSinks.toList yield ValueAtStage(sink, this.get(sink))
		val stateByCalculatedStage = loop(stateBySink, Nil, Map.empty)

		GraphMap.fill(closedGraph)(stateByCalculatedStage.apply)
	}

}

object GraphMap {
	def fill[A](graph: ClosedGraph)(f: Stage => A): GraphMap[A] = new GraphMap[A](graph, graph.stages.map(f))
}
