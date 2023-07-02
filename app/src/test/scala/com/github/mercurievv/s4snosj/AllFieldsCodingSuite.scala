package com.github.mercurievv.s4snosj

import io.circe.Json
import io.circe.Error
import io.circe.parser.*
import io.circe.syntax.*
import io.circe.generic.auto.*
import io.circe.yaml.parser
import cats.implicits.*
import cats.effect.implicits.*
import cats.effect.{IO, Resource}
import cats.kernel.Eq
import fs2.*
import fs2.io.file
import fs2.io.file.Path
import weaver.SimpleIOSuite

import java.nio.file.*
import SimpleDefinitionCodecs.given_Codec_SimpleDefinition

def removeOrdering(fromStr: Either[Error, Json]) = {
  val errorOrString = fromStr.map(_.deepDropNullValues.spaces2SortKeys)
  errorOrString.flatMap(parse)}

object AllFieldsCodingSuite extends SimpleIOSuite:
  def readResource(resourceName: String): Stream[IO, String] =
    val uri = getClass.getResource(resourceName)
    val path = Path(uri.getPath)
    file.Files[IO].readAll(path)
      .through(text.utf8.decode)
      .through(text.lines)


  test("Load a file from resources") {
    val resourceName = "/jsonschema/test1.yaml"
    val lines = readResource(resourceName).compile.toList
    lines
      .map(_.mkString("\n"))
      .map(v =>
        val fromStr = parser.parse(v)
        val obj = fromStr.flatMap(_.as[SimpleDefinition])
        val fromObj = obj.map(_.asJson)
        val errorOrJson = removeOrdering(fromStr)
        val errorOrJson1 = removeOrdering(fromObj)
        expect(fromStr.isRight)
          .and(expect(fromObj.isRight))
          .and(expect.eql(errorOrJson, errorOrJson1))
      )
  }


