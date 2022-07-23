package com.lambdarat

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import PasswordValidator.MIN_PASSWORD_SIZE_VALIDATION

object Generators {

  def fewerOrEqualThan(numberOfChars: Int): Gen[String] =
    Gen
      .listOfN(numberOfChars, Arbitrary.arbChar.arbitrary)
      .map(_.mkString)

  def withOnly(
      minPasswordSize: Int,
      charsOf: Gen[Char],
      maxPasswordSize: Int = 100
  ): Gen[String] =
    for {
      size   <- Gen.choose(minPasswordSize, maxPasswordSize)
      output <- Gen.listOfN(size, charsOf)
    } yield output.mkString

  def withoutCapitalLetter(minPasswordSize: Int): Gen[String] =
    withOnly(minPasswordSize, Gen.alphaLowerChar)

  def withoutLowerCaseLetter(minPasswordSize: Int): Gen[String] =
    withOnly(minPasswordSize, Gen.alphaUpperChar)

  def withoutNumber(minPasswordSize: Int): Gen[String] =
    withOnly(minPasswordSize, Gen.alphaChar)

  def withoutUnderscore(withDigit: Boolean)(minPasswordSize: Int) =
    for {
      basePassword <- withOnly(minPasswordSize, Gen.alphaChar)
      idxs    <- Gen.pick(if (withDigit) 3 else 2, 0 until basePassword.size)
      capital <- Gen.alphaUpperChar
      lower   <- Gen.alphaLowerChar
      digit   <- Gen.alphaNumChar
      replacements =
        if (withDigit)
          List(capital, lower, digit)
        else
          List(capital, lower)
    } yield replacements.zip(idxs).foldLeft(basePassword) {
      case (password, (validChar, idx)) => password.updated(idx, validChar)
    }

}
