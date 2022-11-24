package wms.flow.planner.util

import scala.quoted.{Quotes, Type, Expr}

object TypeIdMacros {

	inline def createId[A]: String = ${createIdImpl[A]}

	def createIdImpl[A](using typeA: Type[A], quotes: Quotes): Expr[String] =
		Expr(Type.show[A])
}
