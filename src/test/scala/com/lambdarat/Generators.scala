package com.lambdarat

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import PasswordValidator.MIN_PASSWORD_SIZE

object Generators {

  lazy val fewerThanEightChars =
    Gen
      .listOfN(MIN_PASSWORD_SIZE - 1, Arbitrary.arbChar.arbitrary)
      .map(_.mkString)

  lazy val withoutCapitalLetter =
    for {
      size   <- Gen.choose(MIN_PASSWORD_SIZE, 100)
      output <- Gen.listOfN(size, Gen.alphaLowerChar)
    } yield output.mkString

  lazy val withoutLowerCaseLetter =
    for {
      size   <- Gen.choose(MIN_PASSWORD_SIZE, 100)
      output <- Gen.listOfN(size, Gen.alphaUpperChar)
    } yield output.mkString

}
