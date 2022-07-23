package com.lambdarat

sealed abstract class PasswordValidationError

object PasswordValidationError {

  case object MinPasswordSize          extends PasswordValidationError
  case object DoesNotContainCapital    extends PasswordValidationError
  case object DoesNotContainLowercase  extends PasswordValidationError
  case object DoesNotContainDigit      extends PasswordValidationError
  case object DoesNotContainUnderscore extends PasswordValidationError

}
