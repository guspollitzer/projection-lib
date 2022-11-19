package wms.flow.planner
package time

import scala.annotation.targetName
import scala.collection.{immutable, mutable}
import scala.collection.immutable.SortedMap
import scala.collection.mutable.ReusableBuilder

opaque type Instant = Float

object Instant {
	given ordering: Ordering[Instant] = Ordering.Float.TotalOrdering
	def mapBuilder[T]: mutable.Builder[(Instant, T), Map[Instant, T]] = SortedMap.newBuilder

	def of(float: Float): Instant = float

	given Conversion[Float, Instant] = identity
}

extension (a: Instant) {

	@targetName("plus")
	def +(b: Duration): Instant = a + b
	@targetName("minus")
	def -(b: Instant): Duration = a - b

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


