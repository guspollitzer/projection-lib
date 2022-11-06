package wms.flow.planner
package funclib

import Validation.*

type Validated[+A] = Validation[String, A]

object Validated {

	type Success[A] = Validation.Success[A]
	type Failure = Validation.Failure[String]
	
	def success[A](a: A): Success[A] = Validation.Success(a)
	def failure(messages: List[String]): Failure = Validation.Failure(messages.head, messages.tail) 

	type ValidatedApplicative = ValidBiasedValidationApplicative[String]
	given validatedApplicative: ValidatedApplicative = validationApplicative[String]
}
