package wms.flow.planner
package resource

import scala.annotation.targetName

opaque type Money = Double

extension (a: Money) {
	@targetName("plus")
	def +(b: Money): Money = a + b
	@targetName("minus")
	def -(b: Money): Money = a - b
}