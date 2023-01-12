package wms.flow.planner
package util

import scala.collection.mutable

class SortedIntArrayBuffer(initialCapacity: Int = 256) {
	private var length: Int = 0;
	private var capacity: Int = initialCapacity
	private var array: Array[Int] = new Array(capacity);

	def size: Int = length

	def addOneInOrder(value: Int): Unit = {
		if length > 0 && value <= array(length - 1) then throw IllegalArgumentException(s"The added value should be greater than the existing ones: value=$value, existing=${array.toSeq}.");
		if length == capacity then {
			capacity *= 2;
			val newArray = new Array[Int](capacity);
			java.lang.System.arraycopy(array, 0, newArray, 0, length);
			array = newArray
		}
		array(length) = value;
		length += 1;
	}

	def addAll(valueIterator: Iterator[Int]): Unit = {
		val values = valueIterator.toArray[Int];
		java.util.Arrays.sort(values);
		values.foreach(addOneInOrder);
	}

	def apply(index: Int): Int = array(index);

	def iterator: Iterator[Int] = array.iterator;

	def contains(value: Int): Boolean = {
		java.util.Arrays.binarySearch(array, value) >= 0
	}

	def containsAny(other: IArray[Int]): Boolean = {
		var index = other.size - 1;
		while index >= 0 && !this.contains(other(index)) do {
			index -= 1
		}
		index >= 0
	}
}
