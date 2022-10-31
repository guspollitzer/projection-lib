package wms.flow.planner
package graph

import scala.collection.immutable

trait Stage {
	def name: String
	def ordinal: Int
}

class Source(val name: String, val ordinal: Int) extends Stage {
	val outPort = new OutPort(this)
}

class Sink(val name: String, val ordinal: Int) extends Stage {
	val inPort = new InPort(this)
}

class Flow(val name: String, val ordinal: Int) extends Stage {
	val inPort = new InPort(this)
	val outPort = new OutPort(this)
}

class Fork2(val name: String, val ordinal: Int) extends Stage {
	val inPort = new InPort(this)
	val outPortA = new OutPort(this)
	val outPortB = new OutPort(this)
}

class Join2(val name: String, val ordinal: Int) extends Stage {
	val inPortA = new InPort(this)
	val inPortB = new InPort(this)
	val outPort = new OutPort(this)
}

