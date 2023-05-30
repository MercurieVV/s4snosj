package com.github.mercurievv.s4snosj

import cats.effect.{IO, IOApp}
import cats.implicits.*

import java.net.InetSocketAddress
import scala.concurrent.ExecutionContext.global
import scala.concurrent.ExecutionContextExecutor

object Main extends IOApp.Simple:

  private implicit val ec: ExecutionContextExecutor = global

  override def run: IO[Unit] =

    val start = IO.canceled

    start
