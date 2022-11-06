package wms.flow.planner
package funclib

trait Applicative[P[_]] extends Functor[P] { self =>
	// primitive combinators

	def map2[A, B, C](fa: P[A], fb: P[B])(f: (A, B) => C): P[C]

	def unit[A](a: A): P[A]

	def lazyUnit[A](a: => A): P[A]

	// derived combinators
	def map[A, B](fa: P[A])(f: A => B): P[B] = map2(fa, unit(()))((a, _) => f(a))

	def traverse[A, B](as: List[A])(f: A => P[B]): P[List[B]] = 
		as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
		
	def sequence[A](lpa: List[P[A]]): P[List[A]] = traverse(lpa)(identity)
	
	def product[A, B](pa: P[A], pb: P[B]): P[(A, B)] = map2(pa, pb)((_, _))

}
