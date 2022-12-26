package wms.flow.planner
package graph

import collection.IterableOnce
import collection.mutable.ArrayBuffer

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

}

/** A closed directed acyclic graph. With "closed" we mean: without any arrow coming from the outside nor going to the outside. */
class ClosedGraph private[ClosedGraph](val stages: IndexedSeq[Stage]) {
	def getSinks: IndexedSeq[SinkN[?]] = stages.collect { case s: SinkN[?] => s }
	def getSources: IndexedSeq[SourceN[?]] = stages.collect { case s: SourceN[?] => s }
}
