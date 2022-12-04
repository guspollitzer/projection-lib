package wms.flow.planner
package queue

import global.Quantity

trait QueueOps[Q] {
	extension (queue: Q) {

		def load: Quantity

		def appended(heap: Heap): Q
		def mergedWith(thatQueue: Q): Q
		def except(thatQueue: Q): Q
		def consumed(quantity: Quantity): Consumption[Q]
	}
}
