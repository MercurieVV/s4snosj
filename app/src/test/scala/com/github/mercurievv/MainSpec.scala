package com.github.mercurievv

import cats.effect.IOApp
import cats.effect.IO

import scala.io.Source

object MainSpec {
  def main(args: Array[String]): Unit =
    val str = Source.fromInputStream(getClass.getResourceAsStream("/openapi/_.yaml")).getLines().mkString("\n")
    println(
      SchemaToCaseClassGenerator.decoder(str) match
        case Right(value) => println(value)
        case Left(error) =>
          error.printStackTrace()
          println(error.getMessage)
          println(error.toString)
    )

}