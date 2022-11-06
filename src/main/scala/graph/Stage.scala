package wms.flow.planner
package graph

import funclib.{Applicative, Validation}

import scala.collection.immutable

object Stage {
	trait Port {
		protected def isOverwired: Boolean
		protected def isUnwired: Boolean

		def incorreclyWiredMessage: Option[String] = {
			if isUnwired then Some("is unwired")
			else if isOverwired then Some("is overwired")
			else None
		}
	}
	trait InPort(val host: Stage) extends Port {
		def from: OutPort
		private[graph] def from_=(in: OutPort): Unit
	}
	trait OutPort(val host: Stage) extends Port {
		def to: InPort
	}
}

trait Stage { self =>
	import Stage.*

	class In private[graph] extends InPort(self) {
		private[graph] var _from: OutPort = _
		def from = _from
		private[graph] def from_=(out: OutPort) = {
			if _from == null
			then _from = out
			else isOverwired = true
		}

		var isOverwired = false;
		override def isUnwired: Boolean = _from == null
	}

	class Out private[graph] extends OutPort(self) {
		private [graph] var _to: InPort = _
		def to = _to

		var isOverwired = false
		override def isUnwired: Boolean = _to == null

		infix def ~>[To](destination: InPort)(using builder: ClosedGraph.Builder): Unit = {
			if _to == null then {
				_to = destination
				destination.from = this
			} else {
				isOverwired = true
			}
		}
	}

	def name: String

	var _ordinal: Int = -1
	def ordinal: Int = _ordinal
	private[graph] def ordinal_=(ord: Int) = _ordinal = ord

	def ports: Map[String, Port]
}

class Source(val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val out = Out()
	val ports = Map("out" -> out)

	builder.register(this)
}

class Sink(val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val in = In()
	val ports = Map("in" -> in)

	builder.register(this)
}

class Flow(val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val in = In()
	val out = Out()
	val ports = Map("in" -> in, "out" -> out)

	builder.register(this)
}

class Fork2(val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val in = In()
	val outA = Out()
	val outB = Out()
	val ports = Map("in" -> in, "outA" -> outA, "outB" -> outB)

	builder.register(this)
}

class Join2(val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val inA = In()
	val inB = In()
	val out = Out()
	val ports = Map("inA" -> inA, "inB" -> inB, "out" -> out)

	builder.register(this)
}
