package wms.flow.planner
package excel

import util.IntArrayBuffer

import scala.collection.{immutable, mutable}

class Sheet(lazinessThreshold: Int = 100) {

	opaque type Reference = Int

	private sealed trait Cell[V](val dependencies: IArray[Reference]) {
		def calcValue(memory: Evaluator): V
	}

	private val cells: mutable.ArrayBuffer[Cell[?]] = mutable.ArrayBuffer.empty

	/** IMPORTANT: the implementations of this trait should be immutable for the eyes of the users. */
	trait Evaluator {
		def eval[V](reference: Reference): V

		def paramsUpdated(paramValueByReference: immutable.Map[Reference, Any]): Evaluator
	}

	private class EagerEvaluator(private[Sheet] val paramValues: IArray[Any], private[Sheet] val cellValues: Array[Any]) extends Evaluator {
		override def eval[V](reference: Reference): V = {
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

		override def paramsUpdated(paramValueByReference: immutable.Map[Reference, Any]): Evaluator = {
			if paramValueByReference.isEmpty then this else {
				val dependantReferences = findDependantsOf(paramValueByReference.iterator.map(entry => entry._1));

				if dependantReferences.size * lazinessThreshold > cells.size then {
					buildEagerEvaluator(this, paramValueByReference, dependantReferences.iterator, mutable.LongMap.empty);
				} else {
					buildLazyEvaluator(this, paramValueByReference, dependantReferences.iterator, mutable.LongMap.empty)
				}
			}
		}
	}


	private class LazyEvaluator(backingEagerEvaluator: EagerEvaluator, overriddenValues: mutable.LongMap[Any]) extends Evaluator {
		override def eval[V](reference: Reference): V = {
			overriddenValues.get(reference) match {
				case None =>
					backingEagerEvaluator.eval(reference)

				case Some(value) =>
					if value != null then value.asInstanceOf[V] else {
						assert(reference >= 0);
						val cellValue = cells(reference).calcValue(this);
						assert(cellValue != null);
						overriddenValues.put(reference, cellValue);
						cellValue.asInstanceOf[V]
					}
			}
		}

		override def paramsUpdated(paramValueByReference: immutable.Map[Reference, Any]): Evaluator = {
			if paramValueByReference.isEmpty then this else {
				val dependantReferences = findDependantsOf(paramValueByReference.iterator.map(entry => entry._1));

				val maxOverriddenSize = 1 + cells.size / lazinessThreshold;
				if (dependantReferences.size > maxOverriddenSize) || overriddenValues.size > maxOverriddenSize then {
					buildEagerEvaluator(backingEagerEvaluator, paramValueByReference, dependantReferences.iterator, overriddenValues)
				} else {
					buildLazyEvaluator(backingEagerEvaluator, paramValueByReference, dependantReferences.iterator, overriddenValues)
				}
			}
		}
	}

	private def buildEagerEvaluator(
		backingEagerEvaluator: EagerEvaluator,
		paramValueByReference: Map[Reference, Any],
		dependantReferences: Iterator[Reference],
		overriddenValues: mutable.LongMap[Any]
	) = {
		val newParamValues = new Array[Any](backingEagerEvaluator.paramValues.length);
		backingEagerEvaluator.paramValues.copyToArray(newParamValues);
		val newCellValues = backingEagerEvaluator.cellValues.clone();
		overriddenValues.foreachEntry {
			(reference, value) =>
				val ref = reference.asInstanceOf[Reference];
				if ref < 0 then newParamValues(~ref) = value else newCellValues(ref) = value
		}
		paramValueByReference.foreachEntry { (ref, value) => newParamValues(~ref) = value }

		for dependantReference <- dependantReferences do {
			if dependantReference >= 0 then newCellValues(dependantReference) = null;
		}
		new EagerEvaluator(IArray.unsafeFromArray(newParamValues), newCellValues)
	}

	private def buildLazyEvaluator(
		backingEagerEvaluator: EagerEvaluator,
		paramValueByReference: Map[Reference, Any],
		dependantReferences: Iterator[Reference],
		alreadyOverriddenValues: mutable.LongMap[Any]
	) = {
		val newOverriddenValues = alreadyOverriddenValues.clone();
		paramValueByReference.foreachEntry { (ref, value) => newOverriddenValues.put(ref, value) }
		dependantReferences.foreach { newOverriddenValues.put(_, null) }
		new LazyEvaluator(backingEagerEvaluator, newOverriddenValues)
	}

	/** Finds the references of the cells that depend, directly or indirectly, on the received references, including themselves.
	  * @return the references of the cells that depend on the received references, including themselves. */
	private def findDependantsOf(referenceIterator: Iterator[Reference]): IntArrayBuffer = {
		val dependantReferences = new IntArrayBuffer(256);
		dependantReferences.addAll(referenceIterator);

		for cellReference <- cells.indices do {
			val cell = cells(cellReference);
			if dependantReferences.containsAny(cell.dependencies) then {
				dependantReferences.addOne(cellReference);
			}
		}
		dependantReferences
	}

	def evaluatorBuilder: EvaluatorBuilder = new EvaluatorBuilder

	class EvaluatorBuilder private[Sheet] {
		private val params: mutable.ArrayBuffer[Any] = mutable.ArrayBuffer.empty

		def addParam[V](value: V): Reference = {
			val newReference = params.size;
			params.addOne(value);
			~newReference
		}

		def complete: Evaluator =
			new EagerEvaluator(IArray.from(params), Array.ofDim(cells.size))
	}

	def addCell0[V](constant: V): Reference =
		addCell(() => new Cell0(constant))

	private class Cell0[V](constant: V) extends Cell[V](IArray()) {
		def calcValue(memory: Evaluator): V = constant
	}

	def addCell1[A, V](refA: Reference)(function: A => V): Reference =
		addCell(() => new Cell1[A, V](refA)(function))

	private class Cell1[A, V](refA: Reference)(function: A => V) extends Cell[V](IArray(refA)) {
		def calcValue(memory: Evaluator): V = {
			val a: A = memory.eval[A](refA);
			function(a)
		}
	}

	def addCell2[A, B, V](refA: Reference, refB: Reference)(function: (A, B) => V): Reference =
		addCell(() => new Cell2[A, B, V](refA, refB)(function))

	private class Cell2[A, B, V](refA: Reference, refB: Reference)(function: (A, B) => V) extends Cell[V](IArray(refA, refB)) {
		def calcValue(memory: Evaluator): V = {
			val a: A = memory.eval[A](refA);
			val b: B = memory.eval[B](refB);
			function(a, b)
		}
	}

	private def addCell[V](cellConstructor: () => Cell[V]): Reference = {
		val newReference = cells.size;
		val newCell = cellConstructor();
		cells.addOne(newCell);
		newReference
	}
}
