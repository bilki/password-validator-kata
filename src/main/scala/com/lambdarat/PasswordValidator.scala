package com.lambdarat

object PasswordValidator {

  def validatePassword(password: String): Boolean = password.size > 8 && password.exists(_.isUpper)

}
