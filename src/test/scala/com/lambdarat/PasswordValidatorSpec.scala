package com.lambdarat

import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.{forAll, forAllNoShrink}

class PasswordValidatorSpec extends FunSuite with ScalaCheckSuite {

  test("All passwords with fewer than 9 chars should validate to false") {
    forAll(Generators.fewerThanEightChars) { password =>
      val result = PasswordValidator.validatePassword(password)

      val expected = false

      assertEquals(result, expected)
    }
  }

  test("All passwords without at least one capital letter should validate to false") {
    forAll(Generators.withoutCapitalLetter) { password =>
      val result = PasswordValidator.validatePassword(password)

      val expected = false

      assertEquals(result, expected)
    }
  }

  test("All passwords without at least one lowercase letter should validate to false") {
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

  test("All passwords without at least one underscore should validate to false") {
    forAll(Generators.withoutUnderscore) { password =>
      val result = PasswordValidator.validatePassword(password)

      val expected = false

      assertEquals(result, expected)
    }
  }

}
