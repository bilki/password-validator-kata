package com.lambdarat

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object Generators {

  lazy val fewerThanEightChars =
    Gen.listOfN(8, Arbitrary.arbChar.arbitrary).map(_.mkString)

}
