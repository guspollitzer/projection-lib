package wms.flow.planner
package excel

import util.SortedIntArrayBuffer
import wms.flow.planner.excel.Sheet.NOT_CALCULATED

import scala.collection.{immutable, mutable}

object Sheet {
	private val NOT_CALCULATED = "Not Calculated";
	private val MAX_DEPTH_LEVEL = 20;
	private val MAP_ENTRY_COST_RELATIVE_TO_ARRAY_ELEMENT = 80;
}

class Sheet(maxDependencyCycleWidth: Int) { thisSheet =>

	import Sheet.*

	private type Reference = Int
	opaque type Ref[T] = Reference

	extension[T] (ref: Ref[T]) {
		def index: Int = ref
	}

	private sealed trait Cell[V](val dependencies: IArray[Reference]) {
		def calcValue(evaluator: Evaluator): V
	}

	private var numberOfParams: Int = 0
	private var watchdog: Int = 0
	private val cells: mutable.ArrayBuffer[Cell[?]] = mutable.ArrayBuffer.empty

	def dependencyCycleWidthWatchdog(): Unit = {
		assert(cells.size - watchdog <= maxDependencyCycleWidth)
		watchdog = cells.size
	}

	def addParam[A](): Ref[A] = {
		val paramRef = ~numberOfParams
		numberOfParams += 1;
		paramRef
	}

	def of[A](constant: A): Ref[A] = {
		addCell {
			() =>
				new Cell[A](IArray()) {
					def calcValue(evaluator: Evaluator): A = constant
				}
		}
	}

	extension[A] (refA: Ref[A]) {
		def map[B](function: A => B): Ref[B] = {
			addCell {
				() =>
					new Cell[B](IArray(refA)) {
						def calcValue(evaluator: Evaluator): B = {
							val a: A = evaluator.eval[A](refA);
							function(a)
						}
					}
			}
		}
	}

	def map2[A, B, C](refA: Ref[A], refB: Ref[B])(function: (A, B) => C): Ref[C] = {
		addCell {
			() =>
				new Cell[C](IArray(refA, refB)) {
					def calcValue(evaluator: Evaluator): C = {
						val a: A = evaluator.eval[A](refA);
						val b: B = evaluator.eval[B](refB);
						function(a, b)
					}
				}
		}
	}

	def map3[A, B, C, D](refA: Ref[A], refB: Ref[B], refC: Ref[C])(function: (A, B, C) => D): Ref[D] = {
		addCell {
			() =>
				new Cell[D](IArray(refA, refB, refC)) {
					def calcValue(evaluator: Evaluator): D = {
						val a: A = evaluator.eval[A](refA);
						val b: B = evaluator.eval[B](refB);
						val c: C = evaluator.eval[C](refC);
						function(a, b, c)
					}
				}
		}
	}

	def map6[A, B, C, D, E, F, G](refA: Ref[A], refB: Ref[B], refC: Ref[C], refD: Ref[D], refE: Ref[E], refF: Ref[F])(function: (A, B, C, D, E, F) => G): Ref[G] = {
		addCell {
			() =>
				new Cell[G](IArray(refA, refB, refC, refD, refE, refF)) {
					def calcValue(evaluator: Evaluator): G = {
						val a: A = evaluator.eval[A](refA);
						val b: B = evaluator.eval[B](refB);
						val c: C = evaluator.eval[C](refC);
						val d: D = evaluator.eval[D](refD);
						val e: E = evaluator.eval[E](refE);
						val f: F = evaluator.eval[F](refF);
						function(a, b, c, d, e, f)
					}
				}
		}
	}

	extension[A] (iterable: Iterable[Ref[A]]) {


		def traverse[B](f: A => B): Ref[Iterable[B]] = {
			addCell {
				() =>
					new Cell[Iterable[B]](IArray.from(iterable)) {
						override def calcValue(evaluator: Evaluator): Iterable[B] =
							for refA <- iterable yield f(evaluator.eval(refA))
					}
			}
		}

		def sequence: Ref[Iterable[A]] = traverse(identity)

		/** The typical foldLeft but renamed to avoid ambiguities. */
		def leftFold[B](zeroRef: Ref[B])(f: (B, A) => B): Ref[B] = {
			addCell {
				() =>
					new Cell[B](IArray.from(iterable ++ List(zeroRef))) {
						def calcValue(evaluator: Evaluator): B = {
							val zero: B = evaluator.eval[B](zeroRef);
							iterable.foldLeft(zero) {
								(accum, refA) =>
									val a: A = evaluator.eval(refA);
									f(accum, a)
							}
						}
					}
			}
		}

		/** The typical reduce but renamed to avoid ambiguities. */
		def collapse(f: (A, A) => A): Ref[A] = {
			if iterable.isEmpty then throw new UnsupportedOperationException("Can't reduce an empty iterable")
			else iterable.tail.leftFold[A](iterable.head)(f)
		}
	}

	private def addCell[V](cellConstructor: () => Cell[V]): Ref[V] = {
		val newReference = cells.size;
		cells.addOne(cellConstructor());
		newReference
	}

	/** Note that the implementations of this trait are immutable for the eyes of the users. */
	sealed trait Evaluator {
		def eval[V](reference: Ref[V]): V

		def paramsUpdated(paramValueByReference: immutable.Map[Reference, Any]): Evaluator

		protected[Sheet] def wasCalculated(cellReference: Reference): Boolean

		protected[Sheet] def copyParamValuesTo(paramValues: Array[Any]): Unit

		protected[Sheet] def copyCellValuesTo(cellValues: Array[Any], len: Int): Unit

		protected[Sheet] def depthLevel: Int

		/** Finds the references of the cells that depend, directly or indirectly, on the received references, including themselves.
		  *
		  * @return the references of the cells that depend on the received references, including themselves. */
		protected[Sheet] def findCalculatedDependantsOf(referenceIterator: Iterator[Reference]): SortedIntArrayBuffer = {
			val dependantReferences = new SortedIntArrayBuffer(256);
			dependantReferences.addAll(referenceIterator);

			var consecutiveNotCalculatedCount = 0;
			var cellIndex = 0;
			while cellIndex < cells.size && consecutiveNotCalculatedCount <= maxDependencyCycleWidth do {
				if wasCalculated(cellIndex) then {
					consecutiveNotCalculatedCount = 0;
					val cell = cells(cellIndex);
					if dependantReferences.containsAny(cell.dependencies) then {
						dependantReferences.addOneInOrder(cellIndex);
					}
				} else {
					consecutiveNotCalculatedCount += 1;
				}
				cellIndex += 1;
			}
			dependantReferences
		}
	}

	def evaluatorBuilder: EvaluatorBuilder = new EvaluatorBuilder

	class EvaluatorBuilder private[Sheet] {
		private val params: Array[Any] = Array.ofDim(numberOfParams)

		def setParam[V](paramRef: Ref[V], paramValue: V): this.type = {
			params(~paramRef) = paramValue;
			this
		}

		def complete: Evaluator = {
			val newCells = Array.ofDim[Any](cells.size);
			java.util.Arrays.fill(newCells, NOT_CALCULATED);
			new EagerEvaluator(IArray.from(params), newCells)
		}
	}

	private class EagerEvaluator(val paramValues: IArray[Any], val cellValues: Array[Any]) extends Evaluator {
		override def wasCalculated(cellReference: Reference): Boolean = cellValues(cellReference).asInstanceOf[AnyRef] ne NOT_CALCULATED;

		override def eval[V](reference: Ref[V]): V = {
			val value =
				if reference < 0 then {
					paramValues(~reference)
				} else {
					var cellValue = cellValues(reference);
					if cellValue.asInstanceOf[AnyRef] eq NOT_CALCULATED then {
						cellValue = cells(reference).calcValue(this)
						cellValues(reference) = cellValue
					}
					cellValue
				}
			value.asInstanceOf[V]
		}

		override def paramsUpdated(paramValueByReference: immutable.Map[Reference, Any]): Evaluator = {
			buildEvaluatorBasedOnAnother(this, paramValueByReference);
		}

		override protected[Sheet] def copyParamValuesTo(target: Array[Any]): Unit = {
			java.lang.System.arraycopy(this.paramValues, 0, target, 0, numberOfParams)
		}

		override protected[Sheet] def copyCellValuesTo(target: Array[Any], len: Int): Unit = {
			java.lang.System.arraycopy(this.cellValues, 0, target, 0, len);
		}

		override protected[Sheet] def depthLevel: Reference = 0
	}

	private class ProxyEvaluator(
		backingEvaluator: Evaluator,
		overriddenParamValueByReference: immutable.Map[Reference, Any],
		lowerDependantIndex: Int,
		upperCellValues: Array[Any],
		val depthLevel: Int
	) extends Evaluator {

		override def wasCalculated(cellReference: Reference): Boolean = {
			if cellReference < lowerDependantIndex then {
				backingEvaluator.wasCalculated(cellReference)
			} else {
				upperCellValues(cellReference - lowerDependantIndex).asInstanceOf[AnyRef] ne NOT_CALCULATED
			}
		}

		override def eval[V](reference: Ref[V]): V = {
			val value =
				if reference < 0 then {
					overriddenParamValueByReference.get(reference) match {
						case None => backingEvaluator.eval(reference)
						case Some(overriddenParamValue) => overriddenParamValue
					}
				} else if reference < lowerDependantIndex then {
					backingEvaluator.eval(reference)
				} else {
					val offset = reference - lowerDependantIndex
					var cellValue = upperCellValues(offset);
					if cellValue.asInstanceOf[AnyRef] eq NOT_CALCULATED then {
						cellValue = cells(reference).calcValue(this);
						upperCellValues(offset) = cellValue;
					}
					cellValue
				}
			value.asInstanceOf[V]
		}

		override def paramsUpdated(paramValueByReference: Map[Reference, Any]): Evaluator = {
			buildEvaluatorBasedOnAnother(this, paramValueByReference)
		}

		override protected[Sheet] def copyParamValuesTo(paramValues: Array[Any]): Unit = {
			backingEvaluator.copyParamValuesTo(paramValues);
			overriddenParamValueByReference.foreachEntry { (reference, value) => paramValues(reference) = value }
		}

		override protected[Sheet] def copyCellValuesTo(cellValues: Array[Any], len: Reference): Unit = {
			backingEvaluator.copyCellValuesTo(cellValues, scala.math.min(len, lowerDependantIndex));
			val diff = len - lowerDependantIndex;
			if diff > 0 then {
				java.lang.System.arraycopy(upperCellValues, 0, cellValues, lowerDependantIndex, diff);
			}
		}
	}

	private def buildEvaluatorBasedOnAnother(
		backingEvaluator: Evaluator,
		overriddenParamValueByReference: immutable.Map[Reference, Any]
	): Evaluator = {
		if overriddenParamValueByReference.isEmpty then {
			backingEvaluator
		} else {
			val dependantReferences = backingEvaluator.findCalculatedDependantsOf(overriddenParamValueByReference.iterator.map(entry => entry._1));
			val dependantCellReferencesIterator = dependantReferences.iterator.filter(_ >= 0);
			val newLowerDependantIndex = if dependantCellReferencesIterator.isEmpty then cells.size else dependantCellReferencesIterator.next();
			// if the maximum depth-level is reached or the memory consumption of a new ProxyEvaluator is greater than of a new EagerEvaluator, then use a EagerEvaluator
			if (
				backingEvaluator.depthLevel >= MAX_DEPTH_LEVEL
					|| overriddenParamValueByReference.size * MAP_ENTRY_COST_RELATIVE_TO_ARRAY_ELEMENT - newLowerDependantIndex > numberOfParams
			) {
				val newParamValues = Array.ofDim[Any](numberOfParams);
				backingEvaluator.copyParamValuesTo(newParamValues);
				overriddenParamValueByReference.foreachEntry { (reference, value) => newParamValues(~reference) = value }

				val newCellValues = Array.ofDim[Any](cells.size);
				backingEvaluator.copyCellValuesTo(newCellValues, cells.size);
				newCellValues(0) = NOT_CALCULATED;
				dependantCellReferencesIterator.foreach { newCellValues(_) = NOT_CALCULATED };

				new EagerEvaluator(IArray.unsafeFromArray(newParamValues), newCellValues)
			} else {
				val newUpperCellValues = Array.ofDim[Any](cells.size - newLowerDependantIndex);
				backingEvaluator.copyCellValuesTo(newUpperCellValues, newLowerDependantIndex);

				newUpperCellValues(0) = NOT_CALCULATED;
				dependantCellReferencesIterator.foreach { reference => newUpperCellValues(reference - newLowerDependantIndex) = NOT_CALCULATED };

				new ProxyEvaluator(backingEvaluator, overriddenParamValueByReference, newLowerDependantIndex, newUpperCellValues, backingEvaluator.depthLevel + 1);
			}
		}
	}
}
