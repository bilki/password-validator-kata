package com.lambdarat

import cats.data.NonEmptyChain
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.kernel.Eq
import cats.syntax.all._
import munit.FunSuite
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.forAllNoShrink

import PasswordValidationError._
import Generators._
import PasswordValidator._

class PasswordValidatorSpec extends FunSuite with ScalaCheckSuite {

  implicit val errorEq: Eq[PasswordValidationError] = Eq.fromUniversalEquals

  case class ValidatorContext(
      name: String,
      validator: String => ValidatedPassword,
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
  val validatePassword4Ctx = ValidatorContext(
    "validatePassword4",
    PasswordValidator.validatePassword4,
    MIN_PASSWORD_SIZE_VALIDATION
  )

  val allValidatorCtx =
    List(validatePasswordCtx, validatePassword2Ctx, validatePassword3Ctx)

  type MinPasswordSizeGen = Int => Gen[String]

  def testValidatorForRule(
      ruleGenerator: MinPasswordSizeGen,
      validatorContexts: List[ValidatorContext],
      title: String,
      expectedError: PasswordValidationError
  ): Unit = {
    validatorContexts
      .map(ctx => ruleGenerator(ctx.minPasswordSize) -> ctx)
      .foreach { case (passwordGen, ctx) =>
        test(
          s"$title for ${ctx.name}"
        ) {
          forAll(passwordGen) { password =>
            val result = ctx.validator(password)

            val expected = expectedError

            result match {
              case Invalid(errors) =>
                assert(clue(errors).contains(clue(expected)))
              case Valid(_) =>
                fail(
                  "Validation returned a correct password instead of errors",
                  clues(result)
                )
            }
          }
        }
      }
  }

  testValidatorForRule(
    fewerOrEqualThan,
    allValidatorCtx,
    "All passwords with fewer or equal number of allowed chars should validate to false",
    MinPasswordSize
  )

  testValidatorForRule(
    withoutCapitalLetter,
    allValidatorCtx,
    "All passwords without at least one capital letter should validate to false",
    DoesNotContainCapital
  )

  testValidatorForRule(
    withoutLowerCaseLetter,
    allValidatorCtx,
    "All passwords without at least one lowercase letter should validate to false",
    DoesNotContainLowercase
  )

  testValidatorForRule(
    withoutNumber,
    List(validatePasswordCtx, validatePassword2Ctx),
    "All passwords without at least one digit should validate to false",
    DoesNotContainDigit
  )

  testValidatorForRule(
    withoutUnderscore(withDigit = true),
    List(validatePasswordCtx),
    "All passwords without at least one underscore should validate to false",
    DoesNotContainUnderscore
  )

  testValidatorForRule(
    withoutUnderscore(withDigit = false),
    List(validatePassword3Ctx),
    "All passwords without at least one underscore should validate to false",
    DoesNotContainUnderscore
  )

  def testValidatorWithValidExamples(
      ctx: ValidatorContext,
      examples: List[String]
  ): Unit =
    examples.foreach(password =>
      test(
        s"${ctx.name} should validate ${password} example as correct"
      ) {
        val result = ctx.validator(password)

        val expected = true

        assertEquals(result.isValid, expected)
      }
    )

  def testValidatorWithInvalidExamples(
      ctx: ValidatorContext,
      examples: Map[String, NonEmptyChain[PasswordValidationError]]
  ): Unit =
    examples.foreach { case (password, expectedErrors) =>
      val expectedErrorsStr = expectedErrors.toChain.toList.mkString(",")

      test(
        s"${ctx.name} should validate ${password} example as incorrect with ${expectedErrorsStr}"
      ) {
        val result = ctx.validator(password)

        val expected = expectedErrors

        result match {
          case Invalid(errors) =>
            assertEquals(errors.size, expected.size)
            assert(clue(expected).forall(clue(errors).contains))
          case Valid(_) =>
            fail(
              "Validation returned a correct password instead of errors",
              clues(result)
            )
        }
      }
    }

  testValidatorWithValidExamples(
    validatePasswordCtx,
    List(
      "abcdE12_B",
      "_1abcdE12",
      "B3abcdE12_",
      "JabcdE12_B&",
      "$abcd_E12a_B"
    )
  )

  testValidatorWithInvalidExamples(
    validatePasswordCtx,
    Map(
      "aB_1"        -> NonEmptyChain(MinPasswordSize),
      "11abcdE12"   -> NonEmptyChain(DoesNotContainUnderscore),
      "BabcdE_jq"   -> NonEmptyChain(DoesNotContainDigit),
      "a1234_"      -> NonEmptyChain(MinPasswordSize, DoesNotContainCapital),
      "%abcd1234_5" -> NonEmptyChain(DoesNotContainCapital)
    )
  )

  testValidatorWithValidExamples(
    validatePassword2Ctx,
    List(
      "abcD123",
      "1Dabc23",
      "DBCab321",
      "&abcA321$",
      "32_1abcE"
    )
  )

  testValidatorWithInvalidExamples(
    validatePassword2Ctx,
    Map(
      "aB1"            -> NonEmptyChain(MinPasswordSize),
      "abcdeFGHIJ"     -> NonEmptyChain(DoesNotContainDigit),
      "BabcdE_jq"      -> NonEmptyChain(DoesNotContainDigit),
      "abcdefg1234577" -> NonEmptyChain(DoesNotContainCapital),
      "%abcd1234_5"    -> NonEmptyChain(DoesNotContainCapital),
      "%ABCIDSEISLIEAS" -> NonEmptyChain(
        DoesNotContainLowercase,
        DoesNotContainDigit
      )
    )
  )

  testValidatorWithValidExamples(
    validatePassword3Ctx,
    List(
      "abcdefghABCDEFGH_",
      "abcdef123ABCDEFGH_",
      "_ABabcdefghABCDEFGH123",
      "%_ABabcdefghABCDEFGH_",
      "$$$$$$aB_$$$$$$$$$$$$$"
    )
  )

  testValidatorWithInvalidExamples(
    validatePassword3Ctx,
    Map(
      "aB_1"                      -> NonEmptyChain(MinPasswordSize),
      "abcdef123ABCDEFGHIJK"      -> NonEmptyChain(DoesNotContainUnderscore),
      "abcdefghijABCDEFGHIJK"     -> NonEmptyChain(DoesNotContainUnderscore),
      "_abcdefghijlkmknksuiudf%%" -> NonEmptyChain(DoesNotContainCapital),
      "______%%ABCDEFGHIJILKLJMM" -> NonEmptyChain(DoesNotContainLowercase),
      "______%%12347234778290089" -> NonEmptyChain(
        DoesNotContainLowercase,
        DoesNotContainCapital
      )
    )
  )

  testValidatorWithValidExamples(
    validatePassword4Ctx,
    List(
      "&&12BC_",   // Min password size
      "12__&&abc", // No capital letters
      "$ab_BC$%&", // No digits
      "BCD123456", // No underscore
      "123ABC___"
    )
  )

  testValidatorWithInvalidExamples(
    validatePassword4Ctx,
    Map(
      "abcB1" -> NonEmptyChain(MinPasswordSize, DoesNotContainUnderscore),
      "abcdEFGH$" -> NonEmptyChain(
        DoesNotContainDigit,
        DoesNotContainUnderscore
      ),
      "__abcde$$" -> NonEmptyChain(DoesNotContainDigit, DoesNotContainCapital),
      "%%1234xxx" -> NonEmptyChain(
        DoesNotContainCapital,
        DoesNotContainUnderscore
      )
    )
  )

}
