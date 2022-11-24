package wms.flow.planner.util

import wms.flow.planner.util.TypeId.{typeIds, registerNew}

import scala.collection.{immutable}

object TypeId {

	@volatile
	private var typeIds: immutable.Set[String] = immutable.HashSet.empty

	def knownTypeIds: immutable.Set[String] = typeIds

	private inline def registerNew[A]: String = {
		val id = TypeIdMacros.createId[A]
		this.synchronized(typeIds += id)
		id
	}
}

trait TypeId[A] {
	val id: String = registerNew[A]

	def equalsTo(that: TypeId[?]): Boolean = this.id == that.id
}
