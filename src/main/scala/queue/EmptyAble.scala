package wms.flow.planner
package queue

trait EmptyAble[C] {
	extension (collection: C) def isEmpty: Boolean
}

object EmptyAble {
	given EmptyAble[FifoQueue] with {
		extension (c: FifoQueue) def isEmpty = c.isEmpty
	}

	given EmptyAble[PriorityQueue] with {
		extension (c: PriorityQueue) def isEmpty = c.isEmpty
	}

	given EmptyAble[Heap] with {
		extension (c: Heap) def isEmpty = c.isEmpty
	}
}
