package com.lambdarat

import munit.{FunSuite, ScalaCheckSuite}
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

  allValidatorCtx
    .map(ctx => fewerOrEqualThan(ctx.minPasswordSize) -> ctx)
    .foreach { case (passwordGen, ctx) =>
      test(
        s"All passwords with fewer or equal number of allowed chars should validate to false for ${ctx.name}"
      ) {
        forAll(passwordGen) { password =>
          val result = ctx.validator(password)

          val expected = false

          assertEquals(result, expected)
        }
      }
    }

  allValidatorCtx
    .map(ctx => withoutCapitalLetter(ctx.minPasswordSize) -> ctx)
    .foreach { case (passwordGen, ctx) =>
      test(
        s"All passwords without at least one capital letter should validate to false for ${ctx.name}"
      ) {
        forAll(passwordGen) { password =>
          val result = ctx.validator(password)

          val expected = false

          assertEquals(result, expected)
        }
      }
    }

  allValidatorCtx
    .map(ctx => withoutLowerCaseLetter(ctx.minPasswordSize) -> ctx)
    .foreach { case (passwordGen, ctx) =>
      test(
        s"All passwords without at least one lowercase letter should validate to false for ${ctx.name}"
      ) {
        forAll(passwordGen) { password =>
          val result = ctx.validator(password)

          val expected = false

          assertEquals(result, expected)
        }
      }
    }

  test("All passwords without at least one digit should validate to false") {
    forAll(Generators.withoutNumber) { password =>
      val result = PasswordValidator.validatePassword(password)

      val expected = false

      assertEquals(result, expected)
    }
  }

  test(
    "All passwords without at least one underscore should validate to false"
  ) {
    forAll(Generators.withoutUnderscore) { password =>
      val result = PasswordValidator.validatePassword(password)

      val expected = false

      assertEquals(result, expected)
    }
  }

}
