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

/** A stage of a graph. */
sealed trait Stage { self =>

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

/** [[Fork]] is a plugin-trait which implements all the [[Stage]]'s output logic. */
trait Fork[B](numberOfOutputs: Int) { stage: Stage =>
	val outs: IndexedSeq[Out[B]] = IndexedSeq.fill(numberOfOutputs)(Out())
	override val outPorts: Map[String, Out[?]] = {
		(for index <- 0 until numberOfOutputs yield {
			s"out${('A' + index).asInstanceOf[Char]}" -> outs(index)
		}).toMap
	}
}

/** [[Join]] is a plugin-trait which implements all the [[Stage]]'s input logic. */
trait Join[A](numberOfInputs: Int) { stage: Stage =>
	val ins: IndexedSeq[In[A]] = IndexedSeq.fill(numberOfInputs)(In());
	override val inPorts: Map[String, In[?]] = {
		(for index <- 0 until numberOfInputs yield {
			s"in${('A' + index).asInstanceOf[Char]}" -> ins(index)
		}).toMap
	}
}


class SourceN[B](val name: String, numberOfOutputs: Int)(using builder: ClosedGraph.Builder) extends Stage, Fork[B](numberOfOutputs) {
	val inPorts: Map[String, In[?]] = Map.empty;

	builder.register(this)
}

class SinkN[A](val name: String, numberOfInputs: Int)(using builder: ClosedGraph.Builder) extends Stage, Join[A](numberOfInputs) {
	val outPorts: Map[String, Out[?]] = Map.empty;

	builder.register(this)
}

class NToM[A, B](val name: String, numberOfInputs: Int, numberOfOutputs: Int)(using builder: ClosedGraph.Builder) extends Stage, Join[A](numberOfInputs), Fork[B](numberOfOutputs) {

	builder.register(this)
}


class Source[A](name: String)(using builder: ClosedGraph.Builder) extends SourceN[A](name, 1) {
	def out: Out[A] = outs(0);
}

class Sink[A](name: String)(using builder: ClosedGraph.Builder) extends SinkN[A](name, 1) {
	def in: In[A] = ins(0);
}

class Flow[A, B](name: String)(using builder: ClosedGraph.Builder) extends NToM[A, B](name, 1, 1) {
	def in: In[A] = ins(0)
	def out: Out[B] = outs(0)
}