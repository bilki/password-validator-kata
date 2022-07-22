package com.lambdarat

import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.{forAll, forAllNoShrink}

class PasswordValidatorSpec extends FunSuite with ScalaCheckSuite {

  test("Property test") {
    forAll { number: Int =>
      assertEquals(-(-(number)), number)
    }
  }

}
