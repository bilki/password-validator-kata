package com.lambdarat

import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.{forAll, forAllNoShrink}

import Generators._

import PasswordValidator.MIN_PASSWORD_SIZE

class PasswordValidatorSpec extends FunSuite with ScalaCheckSuite {

  List(
    fewerOrEqualThan(MIN_PASSWORD_SIZE) -> PasswordValidator.validatePassword _,
    fewerOrEqualThan(6)  -> PasswordValidator.validatePassword2 _,
    fewerOrEqualThan(16) -> PasswordValidator.validatePassword3 _
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
