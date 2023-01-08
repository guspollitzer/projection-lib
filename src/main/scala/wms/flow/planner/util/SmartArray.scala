package wms.flow.planner
package util

class SmartArray[A](defaultValue: => A) {
	var lowerIndex: Int = 0
	var higherIndex: Int = 0

	def apply(index: Int): A = ???
	def set(index: Int, value: A): Unit = ???

}
