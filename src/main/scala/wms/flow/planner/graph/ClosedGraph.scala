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
class ClosedGraph private[ClosedGraph](stages: IndexedSeq[Stage]) {
	closedGraph =>

	import ClosedGraph.*

	def size: Int = stages.size

	def getSinks: IndexedSeq[SinkN[?]] = stages.collect { case s: SinkN[?] => s }

	def getSources: IndexedSeq[SourceN[?]] = stages.collect { case s: SourceN[?] => s }

	/** Creates a new [[Mapping]] that associates every stage of this [[ClosedGraph]] to the result of applying the received function to the corresponding stage */
	def createMapping[A](f: Stage => A): Mapping[A] = new Mapping[A](closedGraph.stages.map(f))

	def fromIterable[A](iterable: Iterable[A]): Mapping[A] = {
		assert(iterable.size == stages.size);
		Mapping(iterable.toIndexedSeq)
	};
	/** Combines two [[Mapping]]s
	  *
	  * @return a new [[Mapping]] in which, for every stage s, its associated value is {{{ f(ma.get(s), mb.get(s)) }}} */
	def combine2Mappings[A, B, C](ma: Mapping[A], mb: Mapping[B])(f: (A, B) => C): Mapping[C] = {
		val cValues = for i <- stages.indices yield f(ma.values(i), mb.values(i));
		Mapping(cValues);
	}

	/** Combines three [[Mapping]]s
	  *
	  * @return a new [[Mapping]] in which, for every stage s, its associated value is {{{ f(ma.get(s), mb.get(s), mc.get(s)) }}} */
	def combine3Mappings[A, B, C, D](ma: Mapping[A], mb: Mapping[B], mc: Mapping[C])(f: (A, B, C) => D): Mapping[D] = {
		val dValues = for i <- stages.indices yield f(ma.values(i), mb.values(i), mc.values(i));
		Mapping(dValues);
	}


	/** A map whose keys are all the stages of this [[ClosedGraph]] instance and the values are of the specified type. */
	case class Mapping[+A](values: IndexedSeq[A]) extends (Stage => A) {
		assert(values.size == closedGraph.stages.size);

		override def apply(stage: Stage): A = {
			assert(stages.contains(stage));
			values.apply(stage.ordinal)
		}

		def map[B](f: A => B): Mapping[B] = Mapping(values.map(f))

		def mapWithStage[B](f: (Stage, A) => B): Mapping[B] = Mapping(stages.map(stage => f(stage, values(stage.ordinal))))

		def foreach(f: A => Unit): Unit = values.foreach(f)

		def forEachWithStage(f: (Stage, A) => Unit): Unit = stages.foreach { stage => f(stage, values(stage.ordinal)) }

		def iterator: Iterator[A] = values.iterator

		def toSeq: Seq[A] = values

		def iteratorWithStage: Iterator[(Stage, A)] = stages.iterator.map { stage => (stage, values(stage.ordinal)) }

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
							val b = f(head.stage, head.value, alreadyCalculated)
							loop(tail, tried, alreadyCalculated + (head.stage -> b))
						}
				}
			}

			val oldValueByStage = for stage <- closedGraph.stages yield ValueAtStage(stage, this(stage))
			val newValueByStage = loop(oldValueByStage.toList, Nil, Map.empty)

			createMapping(newValueByStage.apply)
		}
	}

}
