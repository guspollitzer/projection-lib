package wms.flow.planner
package util

import global.Quantity
import util.TypeId.{registerNew, typeIds}

import scala.collection.immutable

object TypeId {

	@volatile
	private var typeIds: immutable.Set[String] = immutable.HashSet.empty

	def knownTypeIds: immutable.Set[String] = typeIds

	private inline def registerNew[A]: String = {
		val id = TypeIdMacros.createId[A]
		this.synchronized(typeIds += id)
		id
	}

	given TypeId[Quantity] = new TypeId[Quantity]{}
}

trait TypeId[A] {
	val id: String = registerNew[A]

	def equalsTo(that: TypeId[?]): Boolean = this.id == that.id
}
