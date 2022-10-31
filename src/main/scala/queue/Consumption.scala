package wms.flow.planner
package queue

import global.Quantity

/** @param remaining the remaining portion of the queue. Should be empty if [[ignored]] is greater than zero.
 *  @param consumed the consumed portion of the queue.
 *  @param ignored the remaining quantity to consume. */
case class Consumption[Q : EmptyAble](remaining: Q, consumed: Q, ignored: Quantity) {
	assert(ignored == 0 || remaining.isEmpty)
}
