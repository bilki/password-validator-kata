package com.lambdarat

object PasswordValidator {

  val MIN_PASSWORD_SIZE = 9

  val sizeRule: String => Boolean      = _.size >= MIN_PASSWORD_SIZE
  val upperCaseRule: String => Boolean = _.exists(_.isUpper)
  val lowerCaseRule: String => Boolean = _.exists(_.isLower)
  val digitRule: String => Boolean     = _.exists(_.isDigit)

  def validatePassword(password: String): Boolean =
    sizeRule(password) && upperCaseRule(password) &&
      lowerCaseRule(password) && digitRule(password)

}
