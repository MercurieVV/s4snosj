package com.github.mercurievv.s4snosj

import cats.Show
import cats.data.NonEmptyList
import cats.effect.IO
import org.scalacheck.Arbitrary
import weaver.*
import weaver.scalacheck.*
import org.scalacheck.Arbitrary.*
import weaver.Expectations.Helpers.expect
//import org.scalacheck.Prop.forAll
import io.circe.parser.*
import io.circe.syntax.*
import io.circe.generic.auto.*
//import io.circe.jawn.*
import org.scalacheck.Gen
import cats.Id
import cats.implicits.*
import org.scalacheck.cats.implicits.*

object CodecsTest extends SimpleIOSuite with Checkers:

  import ScalacheckDerivation.given
  import ScalacheckDerivation.*
  given Show[Root]                 = Show.fromToString
  given Arbitrary[Discriminator]   = ScalacheckDerivation.ArbitraryDer.derived[Discriminator].toArbitrary
  given Arbitrary[Reference]       = ScalacheckDerivation.ArbitraryDer.derived[Reference].toArbitrary
  given Arbitrary[RegularProperty] = ScalacheckDerivation.ArbitraryDer.derived[RegularProperty].toArbitrary
  given Arbitrary[Property] = Gen.oneOf[Property](summon[Arbitrary[Reference]].arbitrary, summon[Arbitrary[RegularProperty]].arbitrary).toArbitrary
  given Arbitrary[Map[PropertyName, Property]] =
    Gen.mapOf[PropertyName, Property](summon[Arbitrary[PropertyName]].arbitrary.product(given_Arbitrary_Property.arbitrary)).toArbitrary
  given Arbitrary[RegularDefinition] = ScalacheckDerivation.ArbitraryDer.derived[RegularDefinition].toArbitrary
  given [T: Arbitrary]: Arbitrary[NonEmptyList[T]] =
    val arb: Gen[T] = summon[Arbitrary[T]].arbitrary
    (arb, Gen.listOf(arb)).mapN((h: T, t: List[T]) => NonEmptyList.apply(h, t)).toArbitrary
  given Arbitrary[XOf] = ScalacheckDerivation.ArbitraryDer.derived[XOf].toArbitrary
  given Arbitrary[Definition] = Gen.either(summon[Arbitrary[RegularDefinition]].arbitrary, summon[Arbitrary[XOf]].arbitrary.filter(_.isValid)).toArbitrary

  val rootGen: Arbitrary[Root]   = ScalacheckDerivation.ArbitraryDer.derived[Root].toArbitrary
  test("Json encoding and decoding should preserve data integrity") {
    forall(rootGen.arbitrary) { (root: Root) =>
      val encodedJson = root.asJson.noSpaces
      val decodedObj  = decode[Root](encodedJson)

      expect(rootGen.asRight[String] == decodedObj)
    }
  }
