package wms.flow.planner
package util

import scala.collection.mutable

class IntArrayBuffer(initialCapacity: Int = 256) {
	private var size: Int = 0;
	private var capacity: Int = initialCapacity
	private var array: Array[Int] = new Array(capacity);

	def addOne(value: Int): Unit = {
		if size == capacity then {
			capacity *= 2;
			val newArray = new Array[Int](capacity);
			array.copyToArray(newArray);
			array = newArray
		}
		array(size) = value;
		size += 1;
	}

	def apply(index: Int): Int = array(index);

	def contains(value: Int): Boolean = {
		var index = array.length - 1;
		while index >= 0 && array(index) != value do {
			index -= 1;
		}
		index >= 0
	}

	def containsAny(other: IArray[Int]): Boolean = {
		var index = other.size - 1;
		while index >= 0 && !this.contains(other(index)) do {
			index -= 1
		}
		index >= 0
	}
}
