package io.higherkindness.ersatz

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.Resource

import com.github.os72.protocjar.{ Protoc => UnsafeProtoc }

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

object OIO {
  def tempFile(prefix: String, suffix: String): Resource[IO, Path] =
    Resource.make(IO {
      val p = Files.createTempFile(prefix, suffix)
      p.toFile.deleteOnExit()
      p
    })(p => IO(Files.delete(p)))
}

object Protoc {

  def descriptor(
    input: Path
  ): IO[Int] =
    OIO.tempFile("proto", "desc").use { desc =>
      IO(
        UnsafeProtoc.runProtoc(Array(
          "--include_source_info",
          s"--descriptor_set_out=${desc.toAbsolutePath}",
          input.toAbsolutePath.toString
        )))
    }
}

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO(println("HELLO!"))
      res <- Protoc.descriptor(Paths.get("Dummy.proto"))
    } yield ExitCode.Success

}
