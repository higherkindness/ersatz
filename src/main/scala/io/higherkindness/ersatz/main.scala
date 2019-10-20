package io.higherkindness.ersatz

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.Resource
import cats.syntax.all._

import com.github.os72.protocjar.{ Protoc => UnsafeProtoc }
import com.google.protobuf.CodedInputStream
import com.google.protobuf.DescriptorProtos.FileDescriptorSet

import java.nio.ByteBuffer
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.nio.channels.FileChannel

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO(println("HELLO!"))
      res <- Protoc.descriptor(
        Paths.get("Dummy.proto"),
        Paths.get(".")
      )
      _ <- IO(println("did we get a valid descriptor?"))
      _ <- IO(println(res))
    } yield ExitCode.Success

}

object OIO {
  def tempFile(prefix: String, suffix: String): Resource[IO, Path] =
    Resource.make(IO {
      val p = Files.createTempFile(prefix, suffix)
      p.toFile.deleteOnExit()
      p
    })(p => IO(Files.delete(p)))


  def loadFile(path: Path): Resource[IO, ByteBuffer] =
    Resource
      .fromAutoCloseable(IO(FileChannel.open(path, StandardOpenOption.READ)))
      .map(channel => channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size))
}

object Protoc {

  def descriptor(
    input: Path,
    protoPath: Path
  ): IO[Int] =
    OIO.tempFile("proto", "desc").use { desc =>
      val runProtoc = IO(UnsafeProtoc.runProtoc(Array(
          "--include_source_info",
          s"--descriptor_set_out=${desc.toAbsolutePath}",
          s"--proto_path=${protoPath.toAbsolutePath}",
          input.toAbsolutePath.toString
      )))

      val parseDescriptor = OIO.loadFile(desc).use { bb =>
        IO(FileDescriptorSet.parseFrom(CodedInputStream.newInstance(bb)))
      }

      runProtoc <* parseDescriptor
    }
}
