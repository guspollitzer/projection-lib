package wms.flow.planner
package resource

opaque type Money = Double

extension (a: Money) {
	def +(b: Money): Money = a + b
	def -(b: Money): Money = a - b
}
