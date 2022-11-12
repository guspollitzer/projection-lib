package wms.flow.planner
package time

import scala.annotation.targetName
import scala.collection.{immutable, mutable}
import scala.collection.immutable.SortedMap
import scala.collection.mutable.ReusableBuilder

opaque type Instant = Float

object Instant {
	given Ordering[Instant] = Ordering.Float.TotalOrdering
	def mapBuilder[T]: mutable.Builder[(Instant, T), Map[Instant, T]] = SortedMap.newBuilder
}

extension (a: Instant) {
	@targetName("plus")
	def +(b: Instant): Instant = a + b
	@targetName("minus")
	def -(b: Instant): Instant = a - b
	@targetName("dividedBy")
	def /(b: Instant): Float = a / b

	@targetName("equalTo")
	def ==(b: Instant): Boolean = a == b
	@targetName("notEqualTo")
	def !=(b: Instant): Boolean = a != b
	@targetName("lessThan")
	def <(b: Instant): Boolean = a < b
	@targetName("greaterThan")
	def >(b: Instant): Boolean = a > b
	@targetName("lessThanOrEqualTo")
	def <=(b: Instant): Boolean = a <= b
	@targetName("greaterThanOrEqualTo")
	def >=(b: Instant): Boolean = a >= b


	def toJavaInstant(startingInstant: java.time.Instant): java.time.Instant =
		startingInstant.plusSeconds(scala.math.round(a))
}


