package com.lambdarat

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

object Generators {

  def strAtLeastNGen(size: Int, limit: Int): Gen[String] =
    for {
      n   <- Gen.choose(size, limit)
      str <- Gen.listOfN(n, arbitrary[Char])
    } yield str.mkString

  lazy val validPasswordGen: Gen[String] =
    for {
      input         <- strAtLeastNGen(9, 100)
      idxs          <- Gen.pick(4, 0 until input.size)
      capitalLetter <- Gen.alphaUpperChar
      lowerLetter   <- Gen.alphaLowerChar
      number        <- Gen.numChar
      replaceChar = idxs.zip(List(capitalLetter, lowerLetter, number, '_'))
    } yield replaceChar.foldLeft(input) { case (acc, (idx, char)) =>
      acc.updated(idx, char)
    }
}
