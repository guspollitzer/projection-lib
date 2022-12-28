package wms.flow.planner
package resource

import scala.annotation.targetName

opaque type Money = Double

val ZERO_MONEY: Money = 0d

extension (a: Money) {
	@targetName("plus")
	def +(b: Money): Money = a + b
	@targetName("minus")
	def -(b: Money): Money = a - b
}