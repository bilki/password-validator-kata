package com.lambdarat

import cats.data.NonEmptyChain
import cats.data.Validated
import cats.data.Validated.invalidNec
import cats.data.Validated.validNec
import cats.data.ValidatedNec
import cats.syntax.all._

import PasswordValidationError._

object PasswordValidator {

  val MIN_PASSWORD_SIZE_VALIDATION  = 8
  val MIN_PASSWORD_SIZE_VALIDATION2 = 6
  val MIN_PASSWORD_SIZE_VALIDATION3 = 16
  val PASSWORD_UNDERSCORE           = '_'

  case class Rule(predicate: String => Boolean, error: PasswordValidationError)

  def sizeRuleWith(min: Int): Rule = Rule(_.size > min, MinPasswordSize)

  val upperCaseRule: Rule = Rule(_.exists(_.isUpper), DoesNotContainCapital)
  val lowerCaseRule: Rule = Rule(_.exists(_.isLower), DoesNotContainLowercase)
  val digitRule: Rule     = Rule(_.exists(_.isDigit), DoesNotContainDigit)
  val underscoreRule: Rule =
    Rule(_.exists(_ == PASSWORD_UNDERSCORE), DoesNotContainUnderscore)

  val validatePasswordRules: List[Rule] = List(
    sizeRuleWith(MIN_PASSWORD_SIZE_VALIDATION),
    upperCaseRule,
    lowerCaseRule,
    digitRule,
    underscoreRule
  )

  type ValidatedPassword = ValidatedNec[PasswordValidationError, String]

  def validatePasswordWithRules(
      password: String,
      rules: List[Rule]
  ): ValidatedPassword = {
    rules.map { rule =>
      if (rule.predicate(password)) validNec(password)
      else invalidNec(rule.error)
    }.combineAll
  }

  def validatePassword(password: String): ValidatedPassword =
    validatePasswordWithRules(password, validatePasswordRules)

  val validatePassword2Rules: List[Rule] = List(
    sizeRuleWith(MIN_PASSWORD_SIZE_VALIDATION2),
    upperCaseRule,
    lowerCaseRule,
    digitRule
  )

  def validatePassword2(password: String): ValidatedPassword =
    validatePasswordWithRules(password, validatePassword2Rules)

  val validatePassword3Rules: List[Rule] = List(
    sizeRuleWith(MIN_PASSWORD_SIZE_VALIDATION3),
    upperCaseRule,
    lowerCaseRule,
    underscoreRule
  )

  def validatePassword3(password: String): ValidatedPassword =
    validatePasswordWithRules(password, validatePassword3Rules)

  val validatePassword4Rules: List[Rule] = List(
    sizeRuleWith(MIN_PASSWORD_SIZE_VALIDATION),
    upperCaseRule,
    digitRule,
    underscoreRule
  )

  def validatePassword4(password: String): ValidatedPassword =
    validatePasswordWithRules(password, validatePassword4Rules)
      .handleErrorWith { nec =>
        if (nec.tail.isEmpty)
          password.validNec
        else
          nec.invalid
      }
}
