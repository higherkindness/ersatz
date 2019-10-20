package io.higherkindness.ersatz

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.Resource
import cats.Traverse
import cats.instances.list._
import cats.syntax.all._

import com.github.os72.protocjar.{ Protoc => UnsafeProtoc }
import com.google.protobuf.CodedInputStream
import com.google.protobuf.DescriptorProtos.FileDescriptorSet
import com.google.protobuf.DescriptorProtos.DescriptorProto
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto
import com.google.protobuf.GeneratedMessageV3

import java.nio.ByteBuffer
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.nio.channels.FileChannel

import scala.collection.JavaConverters._

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

object Proto2Schema {

    val coalgebra: GeneratedMessageV3 => Either[String, SchemaF[GeneratedMessageV3]] =
      _ match {
        case v: DescriptorProto =>
          Right(RootSchemaF(v.getName, v.getFieldList.asScala.toList))

        case v: FieldDescriptorProto =>
          import FieldDescriptorProto.Type._
          (v.getType match {
            case TYPE_STRING => Right(PrimitiveType.String)
            case TYPE_BOOL => Right(PrimitiveType.Boolean)
            case todo => Left(s"Unhandled primitive type $todo")
          }).map(pt => PrimitiveSchemaF(v.getName, pt))

        case other => Left(s"Unable to handle ${other}")
      }
}

sealed trait SchemaF[A] {
}

object SchemaF {
}

final case class RootSchemaF[A](name: String, fields: List[A]) extends SchemaF[A]
final case class PrimitiveSchemaF[A](name: String, primitiveType: PrimitiveType) extends SchemaF[A]

sealed trait PrimitiveType
object PrimitiveType {
  final case object String extends PrimitiveType
  final case object Boolean extends PrimitiveType
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
  ): IO[FileDescriptorSet] =
    OIO.tempFile("proto", "desc").use { desc =>
      val runProtoc = IO(UnsafeProtoc.runProtoc(Array(
          //"--include_source_info",
          s"--descriptor_set_out=${desc.toAbsolutePath}",
          s"--proto_path=${protoPath.toAbsolutePath}",
          input.toAbsolutePath.toString
      )))

      val parseDescriptor = OIO.loadFile(desc).use { bb =>
        IO(FileDescriptorSet.parseFrom(CodedInputStream.newInstance(bb)))
      }

      runProtoc *> parseDescriptor
    }
}
