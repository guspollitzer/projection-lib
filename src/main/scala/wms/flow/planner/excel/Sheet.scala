package wms.flow.planner
package excel

import util.IntArrayBuffer

import scala.collection.{immutable, mutable}

class Sheet {

	opaque type Reference = Int

	private sealed trait Cell[V](val dependencies: IArray[Reference]) {
		def calcValue(memory: Evaluator): V
	}

	private val cells: mutable.ArrayBuffer[Cell[?]] = mutable.ArrayBuffer.empty

	class Evaluator private[Sheet] (private val paramValues: IArray[Any], private val cellValues: Array[Any]) {

		def eval[V](reference: Reference): V = {
			val value =
				if reference < 0 then {
					paramValues(~reference)
				} else {
					var cellValue = cellValues(reference);
					if cellValue == null then {
						cellValue = cells(reference).calcValue(this)
						assert(cellValue != null)
						cellValues(reference) = cellValue
					}
					cellValue
				}
			value.asInstanceOf[V]
		}

		def paramUpdated[V](paramReference: Reference, newValue: V): Evaluator = {
			val newParamValues = paramValues.updated(~paramReference, newValue);
			val newCellValues = cellValues.clone();

			val updatedDependencies = new IntArrayBuffer(256);

			for cellReference <- cellValues.indices do {
				val cell = cells(cellReference);
				if updatedDependencies.containsAny(cell.dependencies) then {
					updatedDependencies.addOne(cellReference);
					newCellValues(cellReference) = null;
				}
			}

			new Evaluator(newParamValues, newCellValues)
		}
	}

	class EvaluatorBuilder private[Sheet] {
		private val params: mutable.ArrayBuffer[Any] = mutable.ArrayBuffer.empty

		def addParam[V](value: V): Reference = {
			val newReference = params.size;
			params.addOne(value);
			~newReference
		}

		def complete: Evaluator =
			new Evaluator(IArray.from(params), Array.ofDim(cells.size))
	}

	def evaluatorBuilder: EvaluatorBuilder = new EvaluatorBuilder

	def addCell0[V](constant: V): Reference =
		addCell(() => new Cell0(constant))

	def addCell1[A, V](refA: Reference)(function: A => V): Reference =
		addCell(() => new Cell1[A, V](refA)(function))

	def addCell2[A, B, V](refA: Reference, refB: Reference)(function: (A, B) => V): Reference =
		addCell(() => new Cell2[A, B, V](refA, refB)(function))

	private def addCell[V](cellConstructor: () => Cell[V]): Reference = {
		val newReference = cells.size;
		val newCell = cellConstructor();
		cells.addOne(newCell);
		newReference
	}

	private class Cell0[V](constant: V) extends Cell[V](IArray()) {
		def calcValue(memory: Evaluator): V = constant
	}

	private class Cell1[A, V](refA: Reference)(function: A => V) extends Cell[V](IArray(refA)) {
		def calcValue(memory: Evaluator): V = {
			val a: A = memory.eval[A](refA);
			function(a)
		}
	}

	private class Cell2[A, B, V](refA: Reference, refB: Reference)(function: (A, B) => V) extends Cell[V](IArray(refA, refB)) {
		def calcValue(memory: Evaluator): V = {
			val a: A = memory.eval[A](refA);
			val b: B = memory.eval[B](refB);
			function(a, b)
		}
	}
}
