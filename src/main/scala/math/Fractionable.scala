package wms.flow.planner
package math

trait Fractionable[A] {
	extension (a: A) def fractionate(fraction: Float): A
}
