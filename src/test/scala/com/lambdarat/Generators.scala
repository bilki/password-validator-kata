package com.lambdarat

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

object Generators {

  def strAtLeastNGen(size: Int, limit: Int): Gen[String] =
    for {
      n    <- Gen.choose(size, limit)
      head <- Gen.listOfN(n, arbitrary[Char])
    } yield head.mkString
  }
