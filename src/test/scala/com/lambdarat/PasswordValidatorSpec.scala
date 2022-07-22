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
    forAll(Generators.greaterThanEightCharsGen) { password =>
      val result = PasswordValidator.validatePassword(password)

      val expected = true

      assertEquals(result, expected)
    }
  }

}
