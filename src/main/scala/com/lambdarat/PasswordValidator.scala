package com.lambdarat

object PasswordValidator {

  val MIN_PASSWORD_SIZE = 9

  type Rule = String => Boolean

  val sizeRule: Rule       = _.size >= MIN_PASSWORD_SIZE
  val upperCaseRule: Rule  = _.exists(_.isUpper)
  val lowerCaseRule: Rule  = _.exists(_.isLower)
  val digitRule: Rule      = _.exists(_.isDigit)
  val underscoreRule: Rule = _.exists(_ == '_')

  val allRules: List[Rule] = List(
    sizeRule,
    upperCaseRule,
    lowerCaseRule,
    digitRule,
    underscoreRule
  )

  def validatePassword(password: String): Boolean =
    allRules.foldLeft(true) { case (isValidPassword, nextRule) =>
      isValidPassword && nextRule(password)
    }

}
