package com.lambdarat

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import PasswordValidator.MIN_PASSWORD_SIZE_VALIDATION

object Generators {

  def fewerOrEqualThan(numberOfChars: Int): Gen[String] =
    Gen
      .listOfN(numberOfChars, Arbitrary.arbChar.arbitrary)
      .map(_.mkString)

  def withoutCapitalLetter(minPasswordSize: Int): Gen[String] =
    for {
      size   <- Gen.choose(minPasswordSize, 100)
      output <- Gen.listOfN(size, Gen.alphaLowerChar)
    } yield output.mkString

  def withoutLowerCaseLetter(minPasswordSize: Int): Gen[String] =
    for {
      size   <- Gen.choose(minPasswordSize, 100)
      output <- Gen.listOfN(size, Gen.alphaUpperChar)
    } yield output.mkString

  def withoutNumber(minPasswordSize: Int): Gen[String] =
    for {
      size   <- Gen.choose(minPasswordSize, 100)
      output <- Gen.listOfN(size, Gen.alphaChar)
    } yield output.mkString

  def withoutUnderscore(withDigit: Boolean)(minPasswordSize: Int) =
    for {
      size         <- Gen.choose(MIN_PASSWORD_SIZE_VALIDATION, 100)
      basePassword <- Gen.listOfN(size, Gen.alphaChar).map(_.mkString)
      idxs         <- Gen.pick(if (withDigit) 3 else 2, 0 until size)
      capital      <- Gen.alphaUpperChar
      lower        <- Gen.alphaLowerChar
      digit        <- Gen.alphaNumChar
      replacements =
        if (withDigit)
          List(capital, lower, digit)
        else
          List(capital, lower)
    } yield replacements.zip(idxs).foldLeft(basePassword) {
      case (password, (validChar, idx)) => password.updated(idx, validChar)
    }

}
