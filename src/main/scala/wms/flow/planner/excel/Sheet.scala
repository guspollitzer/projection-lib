package wms.flow.planner
package excel

import com.sun.tools.classfile.Dependencies
import util.SortedIntArrayBuffer
import wms.flow.planner.excel.Sheet.NOT_CALCULATED

import scala.collection.{immutable, mutable}

object Sheet {
	private val NOT_CALCULATED = "Not Calculated";
	private val MAX_DEPTH_LEVEL = 20;
	private val MAP_ENTRY_COST_RELATIVE_TO_ARRAY_ELEMENT = 80;
}

/** Represents a multi-parameter buildable composite function whose result elements may be obtained separately without causing unnecessary recalculations.
  * Usually, most of the result elements are intermediate arguments: the result of a component function that is used as the argument of another component function.
  *
  * The behaviour of this class is analogous to an excel spreadsheet filled with simple pure functions that compose to form complex ones; but lacking the value in the cells that are the argument of said functions. */
class Sheet(maxDependencyCycleWidth: Int) {
	thisSheet =>

	import Sheet.*

	private type Reference = Int
	/** A reference to, or identifier of, a parameter or result element cell. */
	opaque type Ref[T] = Reference

	extension[T] (ref: Ref[T]) {
		def index: Int = ref
	}

	/** A placeholder of a result element. */
	private sealed trait Cell[V](val dependencies: IArray[Reference]) {
		def calcValue(evaluator: Evaluation): V
	}

	private var numberOfParams: Int = 0
	private var watchdog: Int = 0
	private val cells: mutable.ArrayBuffer[Cell[?]] = mutable.ArrayBuffer.empty

	def dependencyCycleWidthWatchdog(): Unit = {
		assert(cells.size - watchdog <= maxDependencyCycleWidth)
		watchdog = cells.size
	}

	/** Adds a parameter to the function represented by this [[Sheet]].
	  *
	  * @tparam A the type of the parameter value.
	  * @return a reference to the created parameter. */
	def addParam[A](): Ref[A] = {
		val paramRef = ~numberOfParams
		numberOfParams += 1;
		paramRef
	}

	/** Adds a result element cell whose value is constant (depends on nothing).
	  *
	  * @param constant the value of the constant.
	  * @tparam A the type of the value.
	  * @return a reference to the added cell. */
	def of[A](constant: A): Ref[A] = {
		createAndAddCell {
			() =>
				new Cell[A](IArray()) {
					def calcValue(evaluator: Evaluation): A = constant
				}
		}
	}

	extension[A] (refA: Ref[A]) {
		/** Adds a result element cell whose value is the result of applying the specified function to the value contained in the parameter or cell referenced by `refA`.
		  *
		  * @param function the function that is applied to the value in the cell referenced by the received [[Ref]] to obtain the value for the created cell.
		  * @tparam B the type of the value contained by the created cell.
		  * @return a reference to the created cell. */
		def map[B](function: A => B): Ref[B] = {
			createAndAddCell {
				() =>
					new Cell[B](IArray(refA)) {
						def calcValue(evaluator: Evaluation): B = {
							val a: A = evaluator.get[A](refA);
							function(a)
						}
					}
			}
		}
	}

	/** Adds a result element cell whose value is the result of applying the specified two-parameters function to the values contained in the parameters and/or cells referenced by `refA` and `refB`.
	  *
	  * @param function the two-parameters function used to calculate the value for the created cell.
	  * @param refA the reference to the cell that contains the first argument.
	  * @tparam A the type of the first parameter of the received function.
	  * @param refB the reference to the cell that contains the second argument.
	  * @tparam B the type of the second parameter of the received function.
	  * @tparam C the type of the value contained by the created cell.
	  * @return a reference to the created cell. */
	def map2[A, B, C](refA: Ref[A], refB: Ref[B])(function: (A, B) => C): Ref[C] = {
		createAndAddCell {
			() =>
				new Cell[C](IArray(refA, refB)) {
					def calcValue(evaluator: Evaluation): C = {
						val a: A = evaluator.get[A](refA);
						val b: B = evaluator.get[B](refB);
						function(a, b)
					}
				}
		}
	}

	/** Like [[map2]] but for functions with three parameters. */
	def map3[A, B, C, D](refA: Ref[A], refB: Ref[B], refC: Ref[C])(function: (A, B, C) => D): Ref[D] = {
		createAndAddCell {
			() =>
				new Cell[D](IArray(refA, refB, refC)) {
					def calcValue(evaluator: Evaluation): D = {
						val a: A = evaluator.get[A](refA);
						val b: B = evaluator.get[B](refB);
						val c: C = evaluator.get[C](refC);
						function(a, b, c)
					}
				}
		}
	}

	/** Like [[map2]] but for functions with six parameters. */
	def map6[A, B, C, D, E, F, G](refA: Ref[A], refB: Ref[B], refC: Ref[C], refD: Ref[D], refE: Ref[E], refF: Ref[F])(function: (A, B, C, D, E, F) => G): Ref[G] = {
		createAndAddCell {
			() =>
				new Cell[G](IArray(refA, refB, refC, refD, refE, refF)) {
					def calcValue(evaluator: Evaluation): G = {
						val a: A = evaluator.get[A](refA);
						val b: B = evaluator.get[B](refB);
						val c: C = evaluator.get[C](refC);
						val d: D = evaluator.get[D](refD);
						val e: E = evaluator.get[E](refE);
						val f: F = evaluator.get[F](refF);
						function(a, b, c, d, e, f)
					}
				}
		}
	}

	extension[A] (thisIterable: Iterable[Ref[A]]) {

		/** Adds a result element cell whose value is the [[Iterable]] that results of applying the received function to the value contained in each of the parameters or cells referenced by the references contained in this [[Iterable]].
		  *
		  * In other words, converts an {{{Iterable[Ref[A]]}}} into a {{{Ref[Iterable[B]]}}}
		  *
		  * @param f the function that is applied to the parameter or cell referenced by each reference in this [[Iterable]] to obtain the elements that conform the [[Iterable]] contained in the added cell.
		  * @tparam B the type of the elements of the [[Iterable]] contained in the created cell.
		  * @return a reference to the created cell.
		  * */
		def traverse[B](f: A => B): Ref[Iterable[B]] = {
			createAndAddCell {
				() =>
					new Cell[Iterable[B]](IArray.from(thisIterable)) {
						override def calcValue(evaluator: Evaluation): Iterable[B] =
							for refA <- thisIterable yield f(evaluator.get(refA))
					}
			}
		}

		/** Adds a result element cell whose value is an [[Iterable]] of the parameters or cells referenced by the references in this [[Iterable]].
		  * In other words, converts an {{{Iterable[Ref[A]]}}} into a {{{Ref[Iterable[A]]}}} */
		def sequence: Ref[Iterable[A]] = traverse(identity)

		/** Adds a result element cell whose value is the result of applying "foldLeft" to the iterable of parameters or cells referenced by the references in this [[Iterable]].
		  * The typical foldLeft but renamed to avoid ambiguities. */
		def leftFold[B](zeroRef: Ref[B])(f: (B, A) => B): Ref[B] = {
			createAndAddCell {
				() =>
					new Cell[B](IArray.from(thisIterable ++ List(zeroRef))) {
						def calcValue(evaluator: Evaluation): B = {
							val zero: B = evaluator.get[B](zeroRef);
							thisIterable.foldLeft(zero) {
								(accum, refA) =>
									val a: A = evaluator.get(refA);
									f(accum, a)
							}
						}
					}
			}
		}

		/** Adds a result element cell whose value is the result of applying "reduce" to the iterable of parameters or cells referenced by the references in this [[Iterable]].
		  * The typical reduce but renamed to avoid ambiguities. */
		def collapse(f: (A, A) => A): Ref[A] = {
			if thisIterable.isEmpty then throw new UnsupportedOperationException("Can't reduce an empty iterable")
			else thisIterable.tail.leftFold[A](thisIterable.head)(f)
		}
	}

	private def createAndAddCell[V](cellConstructor: () => Cell[V]): Ref[V] = {
		val newReference = cells.size;
		cells.addOne(cellConstructor());
		newReference
	}

	/** Knows the arguments and the result of evaluating the multi-parameter function represented by this [[Sheet]] with them.
	  * The result is an ordered list of values.
	  *
	  * Note that the values contained by implementations of this trait are immutable for the eyes of the users. */
	sealed trait Evaluation {
		/** Obtains the value of the parameter or resulting product-element referenced by the received reference. */
		def get[V](reference: Ref[V]): V

		/** Creates a new [[Evaluation]] in which the arguments (of the function represented by this [[Sheet]]) are the same than this [[Evaluation]] except for the specified ones. */
		def argumentsUpdated(argumentByParamRef: immutable.Map[Reference, Any]): Evaluation

		protected[Sheet] def wasCalculated(cellReference: Reference): Boolean

		protected[Sheet] def copyArgumentsTo(target: Array[Any]): Unit

		protected[Sheet] def copyCellValuesTo(target: Array[Any], from: Reference, len: Int): Unit

		protected[Sheet] def depthLevel: Int

		/** Finds the references of the cells that depend, directly or indirectly, on the received references, including themselves.
		  *
		  * @return the references of the cells that depend on the received references, including themselves. */
		protected[Sheet] def findCalculatedDependantsOf(referenceIterator: Iterator[Reference], startingFrom: Reference): SortedIntArrayBuffer = {
			val dependantReferences = new SortedIntArrayBuffer(256);
			dependantReferences.addAll(referenceIterator);

			var consecutiveNotCalculatedCount = 0;
			var cellIndex = startingFrom;
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

		protected[Sheet] def findFirstDependantOf(dependenciesIterator: Iterator[Reference]): Option[Reference] = {
			val dependencies: Set[Reference] = dependenciesIterator.toSet;
			cells.indices.find { index => cells(index).dependencies.exists(dependencies.contains) }
		}
	}

	def evaluationBuilder: EvaluationBuilder = new EvaluationBuilder

	class EvaluationBuilder private[Sheet] {
		private val arguments: Array[Any] = Array.ofDim(numberOfParams)

		def setArgument[V](paramRef: Ref[V], argument: V): this.type = {
			arguments(~paramRef) = argument;
			this
		}

		def complete: Evaluation = {
			val newCells = Array.ofDim[Any](cells.size);
			java.util.Arrays.fill(newCells, NOT_CALCULATED);
			new AutonomousEvaluation(IArray.from(arguments), newCells)
		}
	}

	private class AutonomousEvaluation(val paramValues: IArray[Any], val cellValues: Array[Any]) extends Evaluation {
		override def wasCalculated(cellReference: Reference): Boolean = cellValues(cellReference).asInstanceOf[AnyRef] ne NOT_CALCULATED;

		override def get[V](reference: Ref[V]): V = {
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

		override def argumentsUpdated(paramValueByReference: immutable.Map[Reference, Any]): Evaluation = {
			buildEvaluatorBasedOnAnother(this, paramValueByReference);
		}

		override protected[Sheet] def copyArgumentsTo(target: Array[Any]): Unit = {
			java.lang.System.arraycopy(this.paramValues, 0, target, 0, numberOfParams)
		}

		override protected[Sheet] def copyCellValuesTo(target: Array[Any], from: Reference, len: Int): Unit = {
			java.lang.System.arraycopy(this.cellValues, from, target, 0, len);
		}

		override protected[Sheet] def depthLevel: Reference = 0
	}

	/** An [[Evaluation]] that is backed by the one it was originated from.
	  *
	  * @param backingEvaluation the [[Evaluation]] from which this [[Evaluation]] originated.
	  * @param overriddenArgumentsByParamRef the arguments that differ respect to the backing [[Evaluation]].
	  * @param lowerDependantIndex index of the first cell that depends on any of the overridden arguments.
	  * @param upperCellValues the values of the cells whose index is greater than or equal to the index of the first cell that depends on any of the overridden arguments.
	  * @param depthLevel the number of [[Evaluation]]s that are backing this instance. */
	private class BackedEvaluation(
		backingEvaluation: Evaluation,
		overriddenArgumentsByParamRef: immutable.Map[Reference, Any],
		lowerDependantIndex: Int,
		upperCellValues: Array[Any],
		val depthLevel: Int
	) extends Evaluation {

		override def wasCalculated(cellReference: Reference): Boolean = {
			if cellReference < lowerDependantIndex then {
				backingEvaluation.wasCalculated(cellReference)
			} else {
				upperCellValues(cellReference - lowerDependantIndex).asInstanceOf[AnyRef] ne NOT_CALCULATED
			}
		}

		override def get[V](reference: Ref[V]): V = {
			val value =
				if reference < 0 then {
					overriddenArgumentsByParamRef.get(reference) match {
						case None => backingEvaluation.get(reference)
						case Some(overriddenParamValue) => overriddenParamValue
					}
				} else if reference < lowerDependantIndex then {
					backingEvaluation.get(reference)
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

		override def argumentsUpdated(paramValueByReference: Map[Reference, Any]): Evaluation = {
			buildEvaluatorBasedOnAnother(this, paramValueByReference)
		}

		override protected[Sheet] def copyArgumentsTo(target: Array[Any]): Unit = {
			backingEvaluation.copyArgumentsTo(target);
			overriddenArgumentsByParamRef.foreachEntry { (reference, value) => target(reference) = value }
		}

		override protected[Sheet] def copyCellValuesTo(target: Array[Any], from: Reference, len: Int): Unit = {
			val diff = len - lowerDependantIndex;
			if diff > 0 then {
				backingEvaluation.copyCellValuesTo(target, from, lowerDependantIndex);
				java.lang.System.arraycopy(upperCellValues, 0, target, lowerDependantIndex, diff);
			} else {
				backingEvaluation.copyCellValuesTo(target, from, len);
			}
		}
	}

	private def buildEvaluatorBasedOnAnother(
		backingEvaluator: Evaluation,
		overriddenParamValueByReference: immutable.Map[Reference, Any]
	): Evaluation = {
		if overriddenParamValueByReference.isEmpty then {
			backingEvaluator
		} else {
			backingEvaluator.findFirstDependantOf(overriddenParamValueByReference.iterator.map(entry => entry._1)) match {
				case None =>
					backingEvaluator

				case Some(firstDependant) =>
					val dependantReferences = backingEvaluator.findCalculatedDependantsOf(overriddenParamValueByReference.iterator.map(entry => entry._1), firstDependant);
					val dependantCellReferencesIterator = dependantReferences.iterator.filter(_ >= 0);
					// if the maximum depth-level is reached or the memory consumption of a new BackedEvaluator is greater than of a new AutonomousEvaluator, then use a AutonomousEvaluator
					if (
						backingEvaluator.depthLevel >= MAX_DEPTH_LEVEL
							|| overriddenParamValueByReference.size * MAP_ENTRY_COST_RELATIVE_TO_ARRAY_ELEMENT - firstDependant > numberOfParams
					) {
						val newParamValues = Array.ofDim[Any](numberOfParams);
						backingEvaluator.copyArgumentsTo(newParamValues);
						overriddenParamValueByReference.foreachEntry { (reference, value) => newParamValues(~reference) = value }

						val newCellValues = Array.ofDim[Any](cells.size);
						backingEvaluator.copyCellValuesTo(newCellValues, 0, cells.size);
						dependantCellReferencesIterator.foreach { newCellValues(_) = NOT_CALCULATED };

						new AutonomousEvaluation(IArray.unsafeFromArray(newParamValues), newCellValues)
					} else {
						val newUpperCellValues = Array.ofDim[Any](cells.size - firstDependant);
						backingEvaluator.copyCellValuesTo(newUpperCellValues, firstDependant, newUpperCellValues.length);

						dependantCellReferencesIterator.foreach { reference => newUpperCellValues(reference - firstDependant) = NOT_CALCULATED };

						new BackedEvaluation(backingEvaluator, overriddenParamValueByReference, firstDependant, newUpperCellValues, backingEvaluator.depthLevel + 1);
					}
			}
		}
	}
}


//object Prueba {
//
//	case class Cell(dependencies: IArray[Int])
//
//	val dependencies = Set(-1, -2, -3, -4, -5, -6, -7, -8 ,-9)
//
//	def main(args: Array[String]): Unit = {
//
//		val cells = Array.fill[Cell](10000000)(new Cell(IArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 11, 12, 13, 14, 15, 16, 17, 18, 19)));
//
//		for iteration <- 1 to 10 do {
//			val elegantStart = System.nanoTime()
//			val elegantResult = elegant(cells)
//			val elegantDuration = (System.nanoTime() - elegantStart) / 1000000;
//
//			val fastStart: Long = System.nanoTime()
//			val fastResult = fast(cells)
//			val fastDuration = (System.nanoTime() - fastStart) / 1000000;
//
//			println(s"Iteration $iteration:\nelegant=\t$elegantDuration - $elegantResult\nfast=\t\t$fastDuration - $fastResult")
//		}
//	}
//
//	def elegant(cells: Array[Cell]): Option[Int] = {
//		cells.indices.find { index => cells(index).dependencies.exists(dependencies.contains) }
//	}
//
//	def fast(cells: Array[Cell]): Option[Int] = {
//		var cellIndex = 0;
//		while cellIndex < cells.length do {
//			val cell = cells(cellIndex);
//			var cellDependencyIndex = cell.dependencies.length;
//			while cellDependencyIndex > 0 do {
//				cellDependencyIndex -= 1;
//				if dependencies.contains(cell.dependencies(cellDependencyIndex)) then return Some(cellIndex)
//			}
//			cellIndex += 1
//		}
//		None
//	}
//
//}