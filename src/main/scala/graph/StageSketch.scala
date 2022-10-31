package wms.flow.planner
package graph


trait StageSketch {
	type Of <: Stage
	def name: String
	def arise(ordinal: Int): Of
}

class SourceSketch(val name: String) extends StageSketch {
	val outPort = new OutPort(this)
	override type Of = Source
	override def arise(ordinal: Int): Of = Source(name, ordinal)
}

class SinkSketch(val name: String) extends StageSketch {
	val inPort = new InPort(this)
	override type Of = Sink
	override def arise(ordinal: Int): Of = Sink(name, ordinal)
}

class FlowSketch(val name: String) extends StageSketch {
	val inPort = new InPort(this)
	val outPort = new OutPort(this)
	override type Of = Flow
	override def arise(ordinal: Int): Of = Flow(name, ordinal)
}

class Fork2Sketch(val name: String) extends StageSketch {
	val inPort = new InPort(this)
	val outPortA = new OutPort(this)
	val outPortB = new OutPort(this)
	override type Of = Fork2
	override def arise(ordinal: Int): Of = Fork2(name, ordinal)
}

class Join2Sketch(val name: String) extends StageSketch {
	val inPortA = new InPort(this)
	val inPortB = new InPort(this)
	val outPort = new OutPort(this)
	override type Of = Join2
	override def arise(ordinal: Int): Of = Join2(name, ordinal)
}