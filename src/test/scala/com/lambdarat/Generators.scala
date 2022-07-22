package com.lambdarat

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import PasswordValidator.MIN_PASSWORD_SIZE

object Generators {

  def fewerOrEqualThan(numberOfChars: Int): Gen[String] =
    Gen
      .listOfN(numberOfChars, Arbitrary.arbChar.arbitrary)
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

  lazy val withoutNumber =
    for {
      size   <- Gen.choose(MIN_PASSWORD_SIZE, 100)
      output <- Gen.listOfN(size, Gen.alphaChar)
    } yield output.mkString

  lazy val withoutUnderscore =
    for {
      size         <- Gen.choose(MIN_PASSWORD_SIZE, 100)
      basePassword <- Gen.listOfN(size, Gen.alphaChar).map(_.mkString)
      idxs         <- Gen.pick(3, 0 until size)
      capital      <- Gen.alphaUpperChar
      lower        <- Gen.alphaLowerChar
      digit        <- Gen.alphaNumChar
      replacements = List(capital, lower, digit).zip(idxs)
    } yield replacements.foldLeft(basePassword) {
      case (password, (validChar, idx)) => password.updated(idx, validChar)
    }

}
