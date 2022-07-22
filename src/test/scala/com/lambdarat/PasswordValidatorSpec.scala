package com.lambdarat

import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.{forAll, forAllNoShrink}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

class PasswordValidatorSpec extends FunSuite with ScalaCheckSuite {

  /*
Have more than 8 characters
Contains a capital letter
Contains a lowercase
Contains a number
Contains an underscore
   */

  test("All passwords with more than 8 characters must be valid") {
    forAll(Generators.strAtLeastNGen(9, 100)) { password =>
      val result = PasswordValidator.validatePassword(password)

      val expected = true

      assertEquals(result, expected)
    }
  }

  test("All passwords with fewer than 9 characters must be invalid") {
    forAll(Generators.strAtLeastNGen(0, 8)) { password =>
      val result = PasswordValidator.validatePassword(password)

      val expected = false

      assertEquals(result, expected)
    }
  }

  test("All password shoud have at least one capital letter") {
    forAll(Generators.validPasswordGen) { password =>
      val result = PasswordValidator.validatePassword(password)

      val expected = true

      assertEquals(result, expected)
    }
  }

}
