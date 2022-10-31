package wms.flow.planner
package queue

import global.Quantity

trait QueueOps[Q] {
	extension (queue: Q) def appended(heap: Heap): Q
	extension (queue: Q) def consumed(quantity: Quantity): Consumption[Q]
}
