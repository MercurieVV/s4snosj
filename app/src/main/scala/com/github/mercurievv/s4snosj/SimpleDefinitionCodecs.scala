package com.github.mercurievv.s4snosj

import cats.data.NonEmptyList
import cats.implicits.*
import com.github.mercurievv.*
import com.github.mercurievv.s4snosj.SimpleDefinition.RegularPropertyCommon
import io.circe
import io.circe.syntax.*
import io.circe.*
import io.circe.Decoder.Result
import io.circe.Encoder.AsArray
import io.circe.cursor.ObjectCursor
import io.circe.derivation.{Configuration, ConfiguredCodec, ConfiguredEnumCodec}
import io.circe.generic.semiauto.*

import scala.reflect.{ClassTag, classTag}

case class Root(
    definitions: Map[DefinitionName, Definition]
)
type Definition     = SimpleDefinition Either XOf Either Reference
type DefinitionName = String

case class XOf(
    allOf: Option[AllOf],
    anyOf: Option[AllOf],
    oneOf: Option[AllOf],
)
type AllOf = NonEmptyList[Either[Reference, SimpleDefinition]]

enum Type:
  case string, number, integer, array, `object`
type PropertyName = String

type DiscriminatorKey = String
case class Discriminator(
    propertyName: PropertyName,
    mapping: Option[Map[DiscriminatorKey, RefPath]],
)

type RefPath = String
case class Reference(`$ref`: RefPath)

object SimpleDefinition:
  case class RegularPropertyCommon(
      `$id`: Option[String],
      `$schema`: Option[String],
      `type`: Type,
      description: Option[String],
      title: Option[String],
  )
enum SimpleDefinition(
    val common: RegularPropertyCommon
):
  case Object(
      override val common: RegularPropertyCommon,
      properties: Map[PropertyName, Definition],
      required: Option[List[PropertyName]],
      discriminator: Option[Discriminator],
      additionalProperties: Option[Boolean],
      definitions: Option[Map[DefinitionName, Definition]],
  ) extends SimpleDefinition(common)
  case Number(
      override val common: RegularPropertyCommon,
      minimum: Option[Int],
      maximum: Option[Int],
  ) extends SimpleDefinition(common)
  case StringT(
      override val common: RegularPropertyCommon,
      minLength: Option[Int],
      maxLength: Option[Int],
      pattern: Option[String],
      `enum`: Option[NonEmptyList[String]],
  ) extends SimpleDefinition(common)
  case ArrayT(
      override val common: RegularPropertyCommon,
      minItems: Option[Int],
      maxItems: Option[Int],
      items: Option[Reference],
  ) extends SimpleDefinition(common)

type SimpleDefinitionByType[T <: Type] = T match
  case Type.number.type   => SimpleDefinition.Number.type
  case Type.integer.type  => SimpleDefinition.Number
  case Type.array.type    => SimpleDefinition.ArrayT.type
  case Type.string.type   => SimpleDefinition.StringT.type
  case Type.`object`.type => SimpleDefinition.Object.type

type Property = Reference | SimpleDefinition | XOf

def simpleDefinitionByType[T <: Type](t: T): ClassTag[SimpleDefinition] =
  def mapToSD[SD <: SimpleDefinition](implicit tag: ClassTag[SD]): ClassTag[SimpleDefinition] =
    ClassTag[SimpleDefinition](tag.runtimeClass)

  t match
    case Type.number   => mapToSD[SimpleDefinition.Number]
    case Type.integer  => mapToSD[SimpleDefinition.Number]
    case Type.array    => mapToSD[SimpleDefinition.ArrayT]
    case Type.string   => mapToSD[SimpleDefinition.StringT]
    case Type.`object` => mapToSD[SimpleDefinition.Object]

extension (a: XOf)
  def isValid: Boolean = a.oneOf.isDefined || a.anyOf.isDefined || a.allOf.isDefined
  def validate: Either[String, XOf] = Either.cond(
    isValid,
    a,
    "'allOf', 'anyOf', 'oneOf' - none of this field is defined",
  )

extension (a: Property)
  def typeName(schema: Root): String = a match
    case v: SimpleDefinition => v.common.`type`.toString
    case Reference(ref) =>
      val refSchema = schema.definitions
        .getOrElse(
          ref.stripPrefix("#/definitions/").stripPrefix("#/components/schemas/"),
          throw new IllegalArgumentException(s"Unknown $ref"),
        )
      // refSchema.`type`
      "Ololo"
    case _ => ""

import io.circe.*
import io.circe.jawn.decode
import io.circe.Json

object SimpleDefinitionCodecs:
  given Codec[Type]                        = ConfiguredEnumCodec.derive()
  given Codec[XOf]                         = deriveCodec[XOf].iemap(_.validate)(identity)
  given Codec[SimpleDefinition Either XOf] = EitherAuto.eitherCodec
  given Codec[Definition]                  = EitherAuto.eitherCodec

  given Codec[Root] = deriveCodec

  given Codec[Discriminator] = deriveCodec

  private val simpleDefinitionByTypeMap: Map[String, String] =
    Type.values.toList
      .fproduct(simpleDefinitionByType)
      .map(_.swap)
      .map((ct, typ) => ct.runtimeClass.getSimpleName -> typ.toString)
      .toMap


  given Codec[SimpleDefinition] =
    given common: Codec[SimpleDefinition.RegularPropertyCommon] = deriveCodec
    given Codec[SimpleDefinition] =
      ConfiguredCodec.derive(transformConstructorNames = simpleDefinitionByTypeMap, discriminator = "type".some)

    FlatteningAggregationCodec.flatteningAggregationCodec[SimpleDefinition, SimpleDefinition.RegularPropertyCommon](
      "common"
    )

  type ReferenceOrSchema = Reference | SimpleDefinition
  given Decoder[ReferenceOrSchema] =
    Decoder[Reference].map(p => p: ReferenceOrSchema).or(Decoder[SimpleDefinition].map(p => p: ReferenceOrSchema))

  given ersd: Codec[Either[Reference, SimpleDefinition]] = EitherAuto.eitherCodec

  given Codec[AllOf] = Codec.from(
    Decoder.decodeNonEmptyList[Either[Reference, SimpleDefinition]],
    Encoder.encodeNonEmptyList[Either[Reference, SimpleDefinition]],
  )

  given Codec[Reference] = deriveCodec

  implicit val propertyDecoder: Decoder[Property] = Decoder[Reference].widen.or(Decoder[SimpleDefinition].widen)
  implicit val propertyEncoder: Encoder[Property] = Encoder.instance {
    case regularProperty: SimpleDefinition => Encoder[SimpleDefinition].apply(regularProperty)
    case refProperty: Reference            => Encoder[Reference].apply(refProperty)
    case XOf(_, _, _)                      => ???
  }

object FlatteningAggregationCodec:
  def flatteningAggregationDecoder[CONTAINER: Decoder, CONTENT: Decoder: Encoder](
      fieldName: String
  ): Decoder[CONTAINER] =
    summon[Decoder[CONTAINER]].prepare(_.withFocus(json =>
      val content = summon[Decoder[CONTENT]].decodeJson(json)
      json.mapObject(_.add(fieldName, content.map(_.asJson).getOrElse(Json.Null)))
    ))

  def flatteningAggregationEncoder[CONTAINER: Encoder](fieldName: String): Encoder[CONTAINER] =
    summon[Encoder[CONTAINER]].mapJson(_.mapObject(js =>
      val nestedJSO = js.apply(fieldName).flatMap(_.asObject).getOrElse(JsonObject.empty)
      js
        .remove(fieldName)
        .deepMerge(nestedJSO)
    ))

  def flatteningAggregationCodec[CONTAINER: Decoder: Encoder, CONTENT: Decoder: Encoder](
      fieldName: String
  ): Codec[CONTAINER] = Codec.from(
    flatteningAggregationDecoder[CONTAINER, CONTENT](fieldName),
    flatteningAggregationEncoder[CONTAINER](fieldName),
  )

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
