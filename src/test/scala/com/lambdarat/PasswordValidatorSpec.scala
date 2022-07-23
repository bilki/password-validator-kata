package com.lambdarat

import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, forAllNoShrink}

import Generators._

import PasswordValidator.{
  MIN_PASSWORD_SIZE_VALIDATION,
  MIN_PASSWORD_SIZE_VALIDATION2,
  MIN_PASSWORD_SIZE_VALIDATION3
}

class PasswordValidatorSpec extends FunSuite with ScalaCheckSuite {

  case class ValidatorContext(
      name: String,
      validator: String => Boolean,
      minPasswordSize: Int
  )

  val validatePasswordCtx = ValidatorContext(
    "validatePassword",
    PasswordValidator.validatePassword,
    MIN_PASSWORD_SIZE_VALIDATION
  )
  val validatePassword2Ctx = ValidatorContext(
    "validatePassword2",
    PasswordValidator.validatePassword2,
    MIN_PASSWORD_SIZE_VALIDATION2
  )
  val validatePassword3Ctx = ValidatorContext(
    "validatePassword3",
    PasswordValidator.validatePassword3,
    MIN_PASSWORD_SIZE_VALIDATION3
  )

  val allValidatorCtx =
    List(validatePasswordCtx, validatePassword2Ctx, validatePassword3Ctx)

  type MinPasswordSizeGen = Int => Gen[String]

  def testValidatorForRule(
      ruleGenerator: MinPasswordSizeGen,
      validatorContexts: List[ValidatorContext],
      title: String
  ): Unit = {
    validatorContexts
      .map(ctx => ruleGenerator(ctx.minPasswordSize) -> ctx)
      .foreach { case (passwordGen, ctx) =>
        test(
          s"$title for ${ctx.name}"
        ) {
          forAll(passwordGen) { password =>
            val result = ctx.validator(password)

            val expected = false

            assertEquals(result, expected)
          }
        }
      }
  }

  testValidatorForRule(
    fewerOrEqualThan,
    allValidatorCtx,
    "All passwords with fewer or equal number of allowed chars should validate to false"
  )

  testValidatorForRule(
    withoutCapitalLetter,
    allValidatorCtx,
    "All passwords without at least one capital letter should validate to false"
  )

  testValidatorForRule(
    withoutCapitalLetter,
    allValidatorCtx,
    "All passwords without at least one lowercase letter should validate to false"
  )

  testValidatorForRule(
    withoutNumber,
    List(validatePasswordCtx, validatePassword2Ctx),
    "All passwords without at least one digit should validate to false"
  )

  testValidatorForRule(
    withoutUnderscore(withDigit = true),
    List(validatePasswordCtx),
    "All passwords without at least one underscore should validate to false"
  )

  testValidatorForRule(
    withoutUnderscore(withDigit = false),
    List(validatePassword3Ctx),
    "All passwords without at least one underscore should validate to false"
  )

  def testValidatorWithExamples(
      ctx: ValidatorContext,
      examples: List[String],
      validatesTo: Boolean
  ): Unit =
    examples.foreach(password =>
      test(
        s"${ctx.name} should validate ${password} example as ${validatesTo}"
      ) {
        val result = ctx.validator(password)

        val expected = validatesTo

        assertEquals(result, expected)
      }
    )

  testValidatorWithExamples(
    validatePasswordCtx,
    List(
      "abcdE12_B",
      "_1abcdE12",
      "B3abcdE12_",
      "JabcdE12_B&",
      "$abcd_E12a_B"
    ),
    validatesTo = true
  )

  testValidatorWithExamples(
    validatePasswordCtx,
    List(
      "aB_1",
      "11abcdE12",
      "BabcdE_jq",
      "abcdefg1234_",
      "%abcd1234_5"
    ),
    validatesTo = false
  )

  testValidatorWithExamples(
    validatePassword2Ctx,
    List(
      "abcD123",
      "1Dabc23",
      "DBCab321",
      "&abcA321$",
      "32_1abcE"
    ),
    validatesTo = true
  )

  testValidatorWithExamples(
    validatePassword2Ctx,
    List(
      "aB1",
      "abcdeFGHIJ",
      "BabcdE_jq",
      "abcdefg1234_",
      "%abcd1234_5"
    ),
    validatesTo = false
  )

  testValidatorWithExamples(
    validatePassword3Ctx,
    List(
      "abcdefghABCDEFGH_",
      "abcdef123ABCDEFGH_",
      "_ABabcdefghABCDEFGH123",
      "%_ABabcdefghABCDEFGH_",
      "$$$$$$aB_$$$$$$$$$$$$$"
    ),
    validatesTo = true
  )

  testValidatorWithExamples(
    validatePassword3Ctx,
    List(
      "aB_1",
      "abcdef123ABCDEFGHIJK",
      "abcdefghijABCDEFGHIJK",
      "_abcdefghijlkmknksuiudf%%",
      "______%%ABCDEFGHIJILKLJMM"
    ),
    validatesTo = false
  )

}
