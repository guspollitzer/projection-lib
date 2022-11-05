package wms.flow.planner
package graph

object Sketch {
	trait InPort(val host: Sketch) {
		def from: OutPort
		private[graph] def from_=(in: OutPort): Unit
	}
	trait OutPort(val host: Sketch) {
		def to: InPort
	}
}

trait Sketch { self =>
	import Sketch.*

	type Of <: Stage
	def name: String
	/** Converts this sketch to a [[Stage]] of the [[Of] type.*/
	def arise(ordinal: Int): Of


	class In extends InPort(self) {
		private[graph] var _from: OutPort = _
		def from = _from
		private[graph] def from_=(in: OutPort) = _from = in
	}

	class Out extends OutPort(self) {
		private[graph] var _to: InPort = _
		def to = _to

		infix def ~>[To](destination: InPort)(using builder: ClosedGraph.Builder): Unit = {
			assert(to == null)
			assert(destination.from == null)
			builder.register(self)
			builder.register(destination.host)
			_to = destination
			destination.from = this
		}
	}
}

package sketchs:

	class Source(val name: String) extends Sketch {
		val outPort = new Out
		override type Of = stages.Source
		override def arise(ordinal: Int): Of = stages.Source(name, ordinal)
	}

	class Sink(val name: String) extends Sketch {
		val inPort = new In
		override type Of = stages.Sink
		override def arise(ordinal: Int): Of = stages.Sink(name, ordinal)
	}

	class Flow(val name: String) extends Sketch {
		val inPort = new In
		val outPort = new Out
		override type Of = stages.Flow
		override def arise(ordinal: Int): Of = stages.Flow(name, ordinal)
	}

	class Fork2(val name: String) extends Sketch {
		val inPort = new In
		val outPortA = new Out
		val outPortB = new Out
		override type Of = stages.Fork2
		override def arise(ordinal: Int): Of = stages.Fork2(name, ordinal)
	}

	class Join2(val name: String) extends Sketch {
		val inPortA = new In
		val inPortB = new In
		val outPort = new Out
		override type Of = stages.Join2
		override def arise(ordinal: Int): Of = stages.Join2(name, ordinal)
	}