package wms.flow.planner
package graph

import collection.IterableOnce
import collection.mutable.ArrayBuffer

object ClosedGraph {

	def build(body: Builder => Unit): Either[List[String], ClosedGraph] = {
		val builder = new Builder()
		body.apply(builder)

		val incorrectlyWiredPorts = for
			stage <- builder.stages
			(portName, port) <- stage.ports
			message <- port.incorreclyWiredMessage
		yield s"${stage.name} - $portName: $message"

		if incorrectlyWiredPorts.isEmpty
		then Right(ClosedGraph(builder.stages.toIndexedSeq))
		else Left(incorrectlyWiredPorts.toList)
	}

	class Builder private[ClosedGraph]() {
		val stages = ArrayBuffer[Stage]()

		def register(stage: Stage): Unit = {
			assert(!stages.contains(stage))
			stage.ordinal = stages.size
			stages.addOne(stage)
		}
	}

}

class ClosedGraph private[ClosedGraph](val stages: IndexedSeq[Stage]) {
	def getSinks: Seq[Sink] = stages.collect { case s: Sink => s }
}
