package com.github.mercurievv.s4snosj

import cats.data.NonEmptyList
import cats.implicits.*
import com.github.mercurievv.*
import io.circe.*
import io.circe.Decoder.Result
import io.circe.Encoder.AsArray
import io.circe.generic.semiauto.*

case class Root(
    definitions: Map[DefinitionName, Definition]
)
type Definition = Either[RegularDefinition, XOf]
type DefinitionName = String

case class XOf(
    allOf: Option[AllOf],
    anyOf: Option[AllOf],
    oneOf: Option[AllOf],
)
type AllOf   = (NonEmptyList[Reference], Option[RegularDefinition])

type Type         = String
type PropertyName = String
case class RegularDefinition(
    `$id`: Option[String],
    `$schema`: Option[String],
    `type`: Type,
    title: Option[String],
    properties: Map[PropertyName, Property],
    required: Option[List[PropertyName]],
    description: Option[String],
    discriminator: Option[Discriminator],
    definitions: Option[Map[DefinitionName, Definition]],
    additionalProperties: Option[Boolean],
)

type DiscriminatorKey = String
case class Discriminator(
    propertyName: PropertyName,
    mapping: Option[Map[DiscriminatorKey, RefPath]],
)

type RefPath = String
case class Reference(`$ref`: RefPath)
case class RegularProperty(
    `type`: Type,
    description: Option[String],
    `enum`: Option[List[String]],
    pattern: Option[String],
    minLength: Option[Int],
    maxLength: Option[Int],
    minItems: Option[Int],
    maxItems: Option[Int],
    minimum: Option[Int],
    items: Option[Reference],
    discriminator: Option[Discriminator],
)

type Property = Reference | RegularProperty | XOf

extension (a: XOf)
  def isValid: Boolean = a.oneOf.isDefined || a.anyOf.isDefined || a.allOf.isDefined
  def validate: Either[String, XOf] = Either.cond(
    isValid,
    a,
    "'allOf', 'anyOf', 'oneOf' - none of this field is defined",
  )

extension (a: Property)
  def typeName(schema: Root): String = a match
    case v: RegularProperty => v.`type`
    case Reference(ref) =>
      val refSchema = schema.definitions
        .getOrElse(
          ref.stripPrefix("#/definitions/").stripPrefix("#/components/schemas/"),
          throw new IllegalArgumentException(s"Unknown $ref"),
        )
      // refSchema.`type`
      "Ololo"
    case _ => ""

object RegularDefinition:
  given Codec[XOf] = deriveCodec[XOf].iemap(_.validate)(identity)
  given Codec[Definition]    = EitherAuto.eitherCodec

  given Codec[Root] = deriveCodec

  given Codec[Discriminator] = deriveCodec

  given Codec[RegularDefinition] = deriveCodec

  type ReferenceOrSchema = Reference | RegularDefinition
  given Decoder[ReferenceOrSchema] =
    Decoder[Reference].map(p => p: ReferenceOrSchema).or(Decoder[RegularDefinition].map(p => p: ReferenceOrSchema))

  val allOfEncoder: Encoder[AllOf] =
    import io.circe.syntax.*
    val encodeRefs = Encoder.encodeNonEmptyList[Reference].contramapArray[AllOf](_._1)
    val encodeDef  = Encoder.encodeList[RegularDefinition].contramapArray[AllOf](_._2.toList)
    (a: AllOf) => {
      val personJson  = a._1.asJson
      val addressJson = a._2.asJson
      personJson.deepMerge(addressJson)
    }

  val allOfDecoder: Decoder[AllOf] = Decoder
    .decodeNonEmptyList[ReferenceOrSchema]
    .emap(refOrSchema =>
      val (schemas: List[RegularDefinition], refs: List[Reference]) = refOrSchema
        .map {
          case r: Reference         => Right(r)
          case s: RegularDefinition => Left(s)
        }
        .toList
        .separate
      NonEmptyList
        .fromList(refs)
        .tupleRight(schemas.headOption)
        .toRight[String]("No refs was found, but I suppose it should be")
    )

  given Codec[AllOf] = Codec.from(
    allOfDecoder,
    allOfEncoder,
  )

  given Codec[RegularProperty] = deriveCodec

  given Codec[Reference] = deriveCodec

  implicit val propertyDecoder: Decoder[Property] = Decoder[Reference].widen.or(Decoder[RegularProperty].widen)
  implicit val propertyEncoder: Encoder[Property] = Encoder.instance {
    case regularProperty: RegularProperty => Encoder[RegularProperty].apply(regularProperty)
    case refProperty: Reference           => Encoder[Reference].apply(refProperty)
    case XOf(_, _, _) => ???
  }

object EitherAuto:
  import io.circe.syntax.*

  given encode[A: Encoder, B: Encoder]: Encoder[Either[A, B]] = Encoder.instance {
    case Left(a)  => a.asJson
    case Right(b) => b.asJson
  }

  given decode[A: Decoder, B: Decoder]: Decoder[Either[A, B]] =
    val decodeLeft  = Decoder[A].map(Left(_))
    val decodeRight = Decoder[B].map(Right(_))
    decodeLeft.widen.handleErrorWith(failure =>
      decodeRight.widen.handleErrorWith(failure2 => {
        val value: String = s"Both of Either decoders failed: \n  ${failure.toString} \n  ${failure2.toString}"
        Decoder.failedWithMessage[Either[A, B]](value)
      })
    )

  given eitherCodec[A: Encoder: Decoder, B: Encoder: Decoder]: Codec[Either[A, B]] =
    Codec.from[Either[A, B]](decode[A, B], encode[A, B])
