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

}
