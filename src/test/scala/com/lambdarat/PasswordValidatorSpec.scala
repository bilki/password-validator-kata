package com.lambdarat

import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.{forAll, forAllNoShrink}

import Generators._

import PasswordValidator.{
  MIN_PASSWORD_SIZE_VALIDATION,
  MIN_PASSWORD_SIZE_VALIDATION2,
  MIN_PASSWORD_SIZE_VALIDATION3
}

class PasswordValidatorSpec extends FunSuite with ScalaCheckSuite {

  val genForValidatePassword =
    fewerOrEqualThan(
      MIN_PASSWORD_SIZE_VALIDATION
    ) -> PasswordValidator.validatePassword _
  val genForValidatePassword2 = fewerOrEqualThan(
    MIN_PASSWORD_SIZE_VALIDATION2
  ) -> PasswordValidator.validatePassword2 _
  val genForValidatePassword3 = fewerOrEqualThan(
    MIN_PASSWORD_SIZE_VALIDATION3
  ) -> PasswordValidator.validatePassword3 _

  List(
    genForValidatePassword,
    genForValidatePassword2,
    genForValidatePassword3
  ).foreach { case (passwordGen, validator) =>
    test(
      "All passwords with fewer or equal number of allowed chars should validate to false"
    ) {
      forAll(passwordGen) { password =>
        val result = validator(password)

        val expected = false

        assertEquals(result, expected)
      }
    }
  }

  test(
    "All passwords without at least one capital letter should validate to false"
  ) {
    forAll(Generators.withoutCapitalLetter) { password =>
      val result = PasswordValidator.validatePassword(password)

      val expected = false

      assertEquals(result, expected)
    }
  }

  test(
    "All passwords without at least one lowercase letter should validate to false"
  ) {
    forAll(Generators.withoutLowerCaseLetter) { password =>
      val result = PasswordValidator.validatePassword(password)

      val expected = false

      assertEquals(result, expected)
    }
  }

  test("All passwords without at least one digit should validate to false") {
    forAll(Generators.withoutNumber) { password =>
      val result = PasswordValidator.validatePassword(password)

      val expected = false

      assertEquals(result, expected)
    }
  }

  test(
    "All passwords without at least one underscore should validate to false"
  ) {
    forAll(Generators.withoutUnderscore) { password =>
      val result = PasswordValidator.validatePassword(password)

      val expected = false

      assertEquals(result, expected)
    }
  }

}
