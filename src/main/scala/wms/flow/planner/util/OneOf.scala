package wms.flow.planner.util

sealed trait OneOf[+A, +B] {
	def a: A
	def b: B
}

case class CaseA[+A, +B](a: A) extends OneOf[A, B] {
	override def b: B = ???
}
case class CaseB[+A, +B](b: B) extends OneOf[A, B] {
	override def a: A = ???
}