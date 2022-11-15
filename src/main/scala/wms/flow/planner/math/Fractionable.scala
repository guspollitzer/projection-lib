package wms.flow.planner
package math

/** Defines the algebra of fractionable things */
trait Fractionable[A] {
	/** Gets a fraction of the fractionable thing. */
	extension (a: A) def takeFraction(fraction: Float): A
}
