package io.higherkindness.ersatz

import cats.effect.ExitCode
import cats.effect.IO
import cats.syntax.all._

import com.github.os72.protocjar.{ Protoc => UnsafeProtoc }
import com.google.protobuf.CodedInputStream
import com.google.protobuf.DescriptorProtos.FileDescriptorSet

import java.nio.file.Path

object Protoc {

  /** Calls out to the native protoc binary with `args` */
  def run(args: String*): IO[ExitCode] =
    IO(UnsafeProtoc.runProtoc(args.toArray)).map(ExitCode(_))

  /** Converts a .proto file to a descriptor set */
  def descriptor(input: Path, protoPath: Path): IO[FileDescriptorSet] =
    OIO.tempFile("proto", "desc").use { desc =>
      val runProtoc = run(
          //"--include_source_info",
          s"--descriptor_set_out=${desc.toAbsolutePath}",
          s"--proto_path=${protoPath.toAbsolutePath}",
          input.toAbsolutePath.toString
      )

      val parseDescriptor = OIO.loadFile(desc).use(bb =>
        IO(FileDescriptorSet.parseFrom(CodedInputStream.newInstance(bb))))

      runProtoc *> parseDescriptor
    }
}
