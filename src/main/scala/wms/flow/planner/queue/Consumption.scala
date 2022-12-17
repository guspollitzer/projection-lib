package wms.flow.planner
package queue

import global.Quantity

/** The result of consuming a queue.
 *  @param remaining the remaining portion of the queue. Should be empty if [[shortage]] is greater than zero.
 *  @param consumed the consumed portion of the queue.
 *  @param shortage the consumption excess. Is greater than zero when the the queue is shorter than the quantity to consume from it. */
case class Consumption[Q : EmptyAble](remaining: Q, consumed: Q, shortage: Quantity) {
	assert(shortage == 0 || shortage > 0 && remaining.isEmpty)

	def map[B : EmptyAble](f: Q => B): Consumption[B] = Consumption(f(this.remaining), f(this.consumed), this.shortage)
}
