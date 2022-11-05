package wms.flow.planner
package graph

import scala.collection.immutable

object Stage {
	trait InPort {
		def from: OutPort
	}
	trait OutPort {
		def to: InPort
	}
}

trait Stage {
	import Stage.*
	
	def name: String
	def ordinal: Int

	class In private[graph] extends InPort {
		private[graph] var _from: OutPort = _
		def from = _from
	}

	class Out extends OutPort {
		private [graph] var _to: InPort = _
		def to = _to
	}
}

package stages:

	class Source(val name: String, val ordinal: Int) extends Stage {
		val out = Out()
	}
	
	class Sink(val name: String, val ordinal: Int) extends Stage {
		val in = In()
	}
	
	class Flow(val name: String, val ordinal: Int) extends Stage {
		val in = In()
		val out = Out()
	}
	
	class Fork2(val name: String, val ordinal: Int) extends Stage {
		val in = In()
		val outA = Out()
		val outB = Out()
	}
	
	class Join2(val name: String, val ordinal: Int) extends Stage {
		val inA = In()
		val inB = In()
		val out = Out()
	}
	
