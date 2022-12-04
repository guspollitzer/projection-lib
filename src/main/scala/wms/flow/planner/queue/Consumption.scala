package wms.flow.planner
package queue

import global.Quantity

/** The result of consuming a queue.
 *  @param remaining the remaining portion of the queue. Should be empty if [[excess]] is greater than zero.
 *  @param consumed the consumed portion of the queue.
 *  @param excess the consumption excess. Is greater than zero when the the queue is shorter than the quantity to consume from it. */
case class Consumption[Q : EmptyAble](remaining: Q, consumed: Q, excess: Quantity) {
	assert(excess == 0 || excess > 0 && remaining.isEmpty)
}
