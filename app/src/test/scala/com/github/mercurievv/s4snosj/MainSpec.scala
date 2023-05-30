package com.github.mercurievv.s4snosj

import cats.effect.{IO, IOApp}

import scala.io.Source

object MainSpec {
  def main(args: Array[String]): Unit =
    val str = Source.fromInputStream(getClass.getResourceAsStream("/jsonschema/test1.yaml")).getLines().mkString("\n")
    println(
      SchemaToCaseClassGenerator.decoder(str) match
        case Right(value) => println(value)
        case Left(error) =>
          error.printStackTrace()
          println(error.getMessage)
          println(error.toString)
    )

}