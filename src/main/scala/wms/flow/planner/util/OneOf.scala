package wms.flow.planner.util

sealed trait OneOf[+A, +B] {
}

case class CaseA[+A, +B](a: A) extends OneOf[A, B]
case class CaseB[+A, +B](b: B) extends OneOf[A, B]
