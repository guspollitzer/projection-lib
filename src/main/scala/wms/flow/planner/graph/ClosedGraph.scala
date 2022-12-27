package wms.flow.planner
package graph

import collection.IterableOnce
import collection.mutable.ArrayBuffer
import scala.annotation.tailrec

object ClosedGraph {

	def build(body: Builder => Unit): Either[List[String], ClosedGraph] = {
		val builder = new Builder()
		body.apply(builder)

		val incorrectlyTiedPorts = for
			stage <- builder.stages
			(portName, port) <- stage.ports
			message <- port.incorrectlyTiedMessage
		yield s"${stage.name} - $portName: $message"

		if incorrectlyTiedPorts.isEmpty
		then Right(ClosedGraph(builder.stages.toIndexedSeq))
		else Left(incorrectlyTiedPorts.toList)
	}

	class Builder private[ClosedGraph]() {
		private[ClosedGraph] val stages = ArrayBuffer[Stage]()

		def register(stage: Stage): Unit = {
			assert(!stages.contains(stage))
			stage.ordinal = stages.size
			stages.addOne(stage)
		}
	}

	private case class ValueAtStage[T](stage: Stage, value: T)


}

/** A closed directed acyclic graph. With "closed" we mean: without any arrow coming from the outside nor going to the outside. */
class ClosedGraph private[ClosedGraph](val stages: IndexedSeq[Stage]) { closedGraph =>
	import ClosedGraph.*

	def getSinks: IndexedSeq[SinkN[?]] = stages.collect { case s: SinkN[?] => s }
	def getSources: IndexedSeq[SourceN[?]] = stages.collect { case s: SourceN[?] => s }

	/** Creates a new [[Mapping]] that associates every stage of this [[ClosedGraph]] to the result of applying the received function to the corresponding stage */
	def createMapping[A](f: Stage => A): Mapping[A] = new Mapping[A](closedGraph.stages.map(f))

	/** A map whose keys are all the stages of this [[ClosedGraph]] instance and the values are of the specified type. */
	case class Mapping[+A](values: IndexedSeq[A]) {
		assert(values.size == closedGraph.stages.size)

		def get(stage: Stage): A = values.apply(stage.ordinal)

		def map[B](f: (Stage, A) => B): Mapping[B] = {
			val mappedValues = for index <- closedGraph.stages.indices yield f(closedGraph.stages(index), values(index))
			Mapping(mappedValues)
		}

		/** Creates a new [[Mapping]] whose values are calculated applying the function `f` on each value of this instance,
		  * starting with the [[Sink]] stages and continuing with the stages that feed the already calculated stages. */
		def calcUpward[B](f: (stage: Stage, oldValue: A, alreadyCalculated: Map[Stage, B]) => B): Mapping[B] = {
			calcDependentwardly(closedGraph.getSinks.toList)(s => s.downstreamStages)(f)
		}

		/** Creates a new [[Mapping]] whose values are calculated applying the function `f` on each value of this instance,
		  * starting with the [[Source]] stages and continuing with the stages fed by the already calculated stages. */
		def calcDownward[B](f: (stage: Stage, value: A, alreadyCalculated: Map[Stage, B]) => B): Mapping[B] = {
			calcDependentwardly(closedGraph.getSources.toList)(s => s.upstreamStages)(f)
		}


		/** Creates a new [[Mapping]] whose values are calculated applying the function `f` on each value of this instance,
		  * starting with the independent stages and continuing with the stages that depend on the already calculated stages.
		  * The dependency relationship between stages is specified by the `dependencyGetter` function. */
		def calcDependentwardly[B](independentStages: List[Stage])(dependenciesGetter: Stage => Set[Stage])
			(f: (stage: Stage, oldValue: A, alreadyCalculated: Map[Stage, B]) => B)
		: Mapping[B] = {

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

			val oldValueByStage = for stage <- closedGraph.stages yield ValueAtStage(stage, this.get(stage))
			val newValueByStage = loop(oldValueByStage.toList, Nil, Map.empty)

			createMapping(newValueByStage.apply)
		}
	}

}
