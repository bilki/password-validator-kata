package com.lambdarat

import cats.data.ValidatedNec
import cats.data.Validated.{invalidNec, validNec}

object PasswordValidator {

  val MIN_PASSWORD_SIZE_VALIDATION  = 8
  val MIN_PASSWORD_SIZE_VALIDATION2 = 6
  val MIN_PASSWORD_SIZE_VALIDATION3 = 16
  val PASSWORD_UNDERSCORE           = '_'

  type Rule = String => Boolean

  def sizeRuleWith(min: Int): Rule = _.size > min

  val upperCaseRule: Rule  = _.exists(_.isUpper)
  val lowerCaseRule: Rule  = _.exists(_.isLower)
  val digitRule: Rule      = _.exists(_.isDigit)
  val underscoreRule: Rule = _.exists(_ == PASSWORD_UNDERSCORE)

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
    rules.foldLeft(true) { case (isValidPassword, nextRule) =>
      isValidPassword && nextRule(password)
    }

    validNec(password)
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
}
