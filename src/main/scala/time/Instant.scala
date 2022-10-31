package wms.flow.planner
package time

import scala.collection.{immutable, mutable}
import scala.collection.immutable.SortedMap
import scala.collection.mutable.ReusableBuilder

opaque type Instant = Float

object Instant {
	given Ordering[Instant] = Ordering.Float.TotalOrdering
	def mapBuilder[T]: mutable.Builder[(Instant, T), Map[Instant, T]] = SortedMap.newBuilder
}

extension (a: Instant) {
	def +(b: Instant): Instant = a + b
	def -(b: Instant): Instant = a - b
	def /(b: Instant): Float = a / b

	def <(b: Instant): Boolean = a < b
	def >(b: Instant): Boolean = a > b
	def <=(b: Instant): Boolean = a <= b
	def >=(b: Instant): Boolean = a >= b
	def !=(b: Instant): Boolean = a != b


	def toJavaInstant(startingInstant: java.time.Instant): java.time.Instant =
		startingInstant.plusSeconds(scala.math.round(a))
}


