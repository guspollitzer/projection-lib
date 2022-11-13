package wms.flow.planner
package graph


import funclib.{Applicative, Validation}
import queue.PriorityQueue

import scala.annotation.{targetName, threadUnsafe}
import scala.collection.MapView

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
	trait InPort[A](val host: Stage) extends Port {
		def from: OutPort[A]
		private[graph] def from_=(in: OutPort[A]): Unit
	}
	trait OutPort[A](val host: Stage) extends Port {
		def to: InPort[A]
	}
}

import wms.flow.planner.graph.Stage.*

trait Stage { self =>

	class In[A] private[graph] extends InPort[A](self) {
		private[graph] var _from: OutPort[A] = _

		def from: OutPort[A] = _from
		private[graph] def from_=(out: OutPort[A]): Unit = {
			if _from == null
			then _from = out
			else isOvertied = true
		}

		var isOvertied = false
		override def isUntied: Boolean = _from == null
	}

	class Out[A] private[graph] extends OutPort[A](self) {
		private [graph] var _to: InPort[A] = _
		def to: InPort[A] = _to

		var isOvertied: Boolean = false
		override def isUntied: Boolean = _to == null

		@targetName("tieTo")
		infix def ~>(destination: InPort[A])(using builder: ClosedGraph.Builder): Unit = {
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

	def outPorts: Map[String, Out[?]]

	def inPorts: Map[String, In[?]]
	def ports: Map[String, Port] = inPorts ++ outPorts

	@threadUnsafe lazy val downstreamStages: Set[Stage] = outPorts.view.values.map(op => op.to.host).toSet

	@threadUnsafe lazy val upstreamStages: Set[Stage] = inPorts.view.values.map(op => op.from.host).toSet
	
}

class Source(val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val out: Out[PriorityQueue] = Out()
	val inPorts: Map[String, In[?]] = Map.empty
	val outPorts: Map[String, Out[?]] = Map("out" -> out)

	builder.register(this)
}

class Sink(val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val in: In[PriorityQueue] = In()
	val inPorts: Map[String, In[?]] = Map("in" -> in)
	val outPorts: Map[String, Out[?]] = Map.empty

	builder.register(this)
}

class Flow[A, B](val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val in: In[A] = In()
	val out: Out[B] = Out()
	val inPorts: Map[String, In[?]] = Map("in" -> in)
	val outPorts: Map[String, Out[?]] = Map("out" -> out)

	builder.register(this)
}

class Fork2[A, B](val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val in: In[A] = In()
	val outA: Out[B] = Out()
	val outB: Out[B] = Out()
	val inPorts: Map[String, In[?]] = Map("in" -> in)
	val outPorts: Map[String, Out[?]] = Map("outA" -> outA, "outB" -> outB)

	builder.register(this)
}

class Join2[A, B](val name: String)(using builder: ClosedGraph.Builder) extends Stage {
	val inA: In[A] = In()
	val inB: In[A] = In()
	val out: Out[B] = Out()
	val inPorts: Map[String, In[?]] = Map("inA" -> inA, "inB" -> inB)
	val outPorts: Map[String, Out[?]] = Map("out" -> out)

	builder.register(this)
}
