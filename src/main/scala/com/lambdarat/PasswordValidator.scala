package com.lambdarat

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

  def validatePasswordWithRules(password: String, rules: List[Rule]): Boolean =
    rules.foldLeft(true) { case (isValidPassword, nextRule) =>
      isValidPassword && nextRule(password)
    }

  def validatePassword(password: String): Boolean =
    validatePasswordWithRules(password, validatePasswordRules)

  val validatePassword2Rules: List[Rule] = List(
    sizeRuleWith(MIN_PASSWORD_SIZE_VALIDATION2)
  )

  def validatePassword2(password: String): Boolean =
    validatePasswordWithRules(password, validatePassword2Rules)

  val validatePassword3Rules: List[Rule] = List(
    sizeRuleWith(MIN_PASSWORD_SIZE_VALIDATION3)
  )

  def validatePassword3(password: String): Boolean =
    validatePasswordWithRules(password, validatePassword3Rules)
}
