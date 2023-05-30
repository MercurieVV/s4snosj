package com.github.mercurievv

import cats.effect.{IO, IOApp}

import java.net.InetSocketAddress
import scala.concurrent.ExecutionContext.global
import scala.concurrent.ExecutionContextExecutor
import cats.implicits.*

object Main extends IOApp.Simple:

  private implicit val ec: ExecutionContextExecutor = global

  override def run: IO[Unit] =

    val start = IO.canceled

    start
