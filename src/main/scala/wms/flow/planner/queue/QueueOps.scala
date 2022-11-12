package wms.flow.planner
package queue

import global.Quantity

trait QueueOps[Q] {
	extension (queue: Q) {
		def appended(heap: Heap): Q
		def consumed(quantity: Quantity): Consumption[Q]
	}
}
