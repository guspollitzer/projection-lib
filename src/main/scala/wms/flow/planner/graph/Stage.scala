package wms.flow.planner
package graph

import funclib.{Applicative, Validation}

import scala.annotation.targetName
import scala.collection.immutable

object Stage {
	trait Port {
		protected def isOvertied: Boolean
		protected def isUntied: Boolean

		def incorrectlyTiedMessage: Option[String] = {
			if isUntied then Some("is untied")
			else if isOvertied then Some("is overtied")
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

import Stage.*

trait Stage { self =>

	class In private[graph] extends InPort(self) {
		private[graph] var _from: OutPort = _

		def from: OutPort = _from
		private[graph] def from_=(out: OutPort): Unit = {
			if _from == null
			then _from = out
			else isOvertied = true
		}

		var isOvertied = false
		override def isUntied: Boolean = _from == null
	}

	class Out private[graph] extends OutPort(self) {
		private [graph] var _to: InPort = _
		def to: InPort = _to

		var isOvertied: Boolean = false
		override def isUntied: Boolean = _to == null

		@targetName("tieTo")
		infix def ~>[To](destination: InPort)(using builder: ClosedGraph.Builder): Unit = {
			if _to == null then {
				_to = destination
				destination.from = this
			} else {
				isOvertied = true
			}
		}
	}

	def name: String

	var _ordinal: Int = -1
	def ordinal: Int = _ordinal
	private[graph] def ordinal_=(ord: Int): Unit = _ordinal = ord

	def ports: Map[String, Port]
}

class Source(val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val out: Out = Out()
	val ports: Map[String, Port] = Map("out" -> out)

	builder.register(this)
}

class Sink(val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val in: In = In()
	val ports: Map[String, Port] = Map("in" -> in)

	builder.register(this)
}

class Flow(val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val in: In = In()
	val out: Out = Out()
	val ports: Map[String, Port] = Map("in" -> in, "out" -> out)

	builder.register(this)
}

class Fork2(val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val in: In = In()
	val outA: Out = Out()
	val outB: Out = Out()
	val ports: Map[String, Port] = Map("in" -> in, "outA" -> outA, "outB" -> outB)

	builder.register(this)
}

class Join2(val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val inA: In = In()
	val inB: In = In()
	val out: Out = Out()
	val ports: Map[String, Port] = Map("inA" -> inA, "inB" -> inB, "out" -> out)

	builder.register(this)
}
