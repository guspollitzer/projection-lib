package wms.flow.planner.queue

import scala.annotation.targetName

/** A monoid whose domain are queues and whose associative operation is concatenation */
trait Concatenable[Queue] {
	def empty: Queue

	extension (a: Queue) {
		@targetName("concat")
		def ++(b: Queue): Queue
	}
}
