package io.higherkindness.ersatz

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO(println("HELLO!"))
    } yield ExitCode.Success

}
