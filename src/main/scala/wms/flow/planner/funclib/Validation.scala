package wms.flow.planner
package funclib

sealed trait Validation[+E, +A]

object Validation {
	type ValidBiasedValidation[E] = [x] =>> Validation[E, x]
	type ValidBiasedValidationApplicative[E] = Applicative[ValidBiasedValidation[E]]

	case class Failure[E](head: E, tail: List[E] = List.empty) extends Validation[E, Nothing]
	case class Success[A](a: A) extends Validation[Nothing, A]
	
	def validationApplicative[E]: ValidBiasedValidationApplicative[E] = new Applicative[[x] =>> Validation[E, x]] {
		def unit[A](a: A): Validation[E, A] = Success(a)

		def lazyUnit[A](a: => A): Validation[E, A] = Success(a)

		def map2[A, B, C](va: Validation[E, A], vb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
			va match {
				case Success(a) => vb match {
					case Success(b) => Success(f(a,b))
					case _ => vb.asInstanceOf[Failure[E]]
				}
				case Failure(h1, t1) => vb match {
					case Failure(h2, t2) => Failure(h1, h2 +: (t1 ++ t2))
					case _ => va.asInstanceOf[Failure[E]]
				}
			}
		
		def when[A](cond: Boolean)(a: A, e: E): Validation[E, A] = if cond then Success(a) else Failure(e)
		def lazyWhen[A](cond: () => Boolean)(a: A, e: E): Validation[E, A] = if cond() then Success(a) else Failure(e) 	
	}
}

