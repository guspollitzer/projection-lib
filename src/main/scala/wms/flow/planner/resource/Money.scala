package wms.flow.planner
package resource

import scala.annotation.targetName

opaque type Money = Double

val ZERO_MONEY: Money = 0d

extension (a: Money) {
	def plus(b: Money): Money = a + b;
	def minus(b: Money): Money = a - b;
	def multipliedBy(factor: Double): Money = a * factor;
}