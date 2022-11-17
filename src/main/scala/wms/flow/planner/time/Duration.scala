package wms.flow.planner
package time

import scala.annotation.targetName


type Duration = Float

//object Duration {
//	given Ordering[Duration] = Ordering.Float.TotalOrdering
//}

extension (a: Duration) {
//	@targetName("plus")
//	def + (b: Duration): Duration = a + b
//	@targetName("minus")
//	def - (b: Duration): Duration = a - b
//	@targetName("dividedBy")
//	def / (b: Duration): Float = a / b
//
//	@targetName("dividedBy")
//	def * (b: Float): Duration = a * b

	def toJavaDuration: java.time.Duration = java.time.Duration.ofSeconds(scala.math.round(a))
}
