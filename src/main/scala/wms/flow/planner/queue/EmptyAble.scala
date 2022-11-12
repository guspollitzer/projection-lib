package wms.flow.planner
package queue

trait EmptyAble[C] {
	extension (collection: C) {
		def isEmpty: Boolean
		def nonEmpty: Boolean = !isEmpty
	}
}
