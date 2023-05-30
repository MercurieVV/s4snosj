package com.github.mercurievv

import io.circe
import cats.implicits.*
import RegularDefinition.given.given

object SchemaToCaseClassGenerator:

  import io.circe.*
  import io.circe.yaml.parser
  def decodeAndGenerate(json: String): Either[Error, String] = decoder(json).map(generates)
  def decoder(json: String): Either[circe.Error, Root] = parser.parse(json).flatMap(_.as[Root])

  def generates(schema: Root): String = ""/*schema.definitions.map((k,v) => {
    v.copy(`$id` = v.`$id`.orElse(k.some))
    generate(schema)(v)
  }).mkString("\n")*/

  def generate(root: Root)(schema: RegularDefinition): String =
    val className = schema.`$id`.getOrElse("GeneratedClass")
    val fields = schema.properties
      .map { case (name, property) =>
        s"  $name: ${typeMapping(property.typeName(root))}"
      }
      .mkString(",\n")

    s"""case class $className(
       |$fields
       |)
       |""".stripMargin

  private def typeMapping(jsonType: String): String = jsonType match
    case "string"  => "String"
    case "integer" => "Int"
    case "number"  => "Double"
    case "boolean" => "Boolean"
    case "array"   => "List[_]"
    case "object"  => "Map[String, _]"
    case _         => throw new IllegalArgumentException(s"Unsupported JSON type: $jsonType")