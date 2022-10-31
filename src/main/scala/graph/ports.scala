package wms.flow.planner
package graph

class InPort[H] private[graph](val host: H) {
	private[graph] var source: OutPort[H] = _
}

class OutPort[H](host: H) {
	var sink: InPort[H] = null

	def ->(destination: InPort[H]): Unit = {
		assert(sink == null)
		assert(destination.source == null)
		sink = destination
		destination.source = this
	}

}