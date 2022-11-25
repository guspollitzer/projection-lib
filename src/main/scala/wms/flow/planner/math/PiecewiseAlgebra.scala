package wms.flow.planner
package math

import time.*
import util.TypeId

trait PiecewiseAlgebra {

	trait Trajectory[+A: TypeId] {

		def numberOfPieces: Int
		// TODO cuando compile probar exponer `wholePieceIntegralByIndex`
		def getWholePieceIntegralAt(index: Int): A

		def integrate(from: Instant, to: Instant): A

		def map[B: TypeId](f: A => B): Trajectory[B]

		def richMap[B: TypeId](f: (index: Int, startingInstant: Instant, endingInstant: Instant, wholePieceIntegral: A) => B): Trajectory[B]

		def combineWith[B, C: TypeId](that: Trajectory[B])(f: (A, B) => C): Trajectory[C] = combine(this, that)(f)

		def richCombineWith[B, C: TypeId](that: Trajectory[B])(f: (index: Int, startingInstant: Instant, endingInstant: Instant, a:A, b: B) => C): Trajectory[C] = richCombine(this, that)(f)

	}

	def buildTrajectory[A: TypeId](wholeIntegralByStepIndex: IterableOnce[A]): Trajectory[A]

	def combine[A, B, C: TypeId](ta: Trajectory[A], tb: Trajectory[B])(f: (A, B) => C): Trajectory[C]

	def richCombine[A, B, C: TypeId](ta: Trajectory[A], tb: Trajectory[B])(f: (index: Int, startingInstant: Instant, endingInstant: Instant, a: A, b: B) => C): Trajectory[C]
}
