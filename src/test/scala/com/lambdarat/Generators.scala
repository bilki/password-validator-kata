package com.lambdarat

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object Generators {

  lazy val fewerThanEightChars =
    Gen.listOfN(8, Arbitrary.arbChar.arbitrary).map(_.mkString)

  lazy val withoutCapitalLetter =
    for {
      size   <- Gen.choose(9, 100)
      output <- Gen.listOfN(size, Gen.alphaLowerChar)
    } yield output.mkString

  lazy val withoutLowerCaseLetter =
    for {
      size   <- Gen.choose(9, 100)
      output <- Gen.listOfN(size, Gen.alphaUpperChar)
    } yield output.mkString

}
