package com.lambdarat

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

object Generators {

  lazy val greaterThanEightCharsGen: Gen[String] =
    for {
      n    <- Gen.choose(9, 100)
      head <- Gen.listOfN(n, arbitrary[Char])
    } yield head.mkString
  }
