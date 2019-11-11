package io.higherkindness.ersatz

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all._
import com.github.os72.protocjar.{Protoc => UnsafeProtoc}
import com.google.protobuf.CodedInputStream
import com.google.protobuf.DescriptorProtos.{FileDescriptorProto, FileDescriptorSet}
import io.higherkindness.ersatz.proto.ProtoParser

import scala.collection.JavaConverters._

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- OIO.println(" ~ ersatz ~ ")

      fileDescriptorSet <- Protoc.descriptor(
        Paths.get("Dummy.proto"),
        Paths.get(".")
      )
      _ <- OIO.println("did we get a valid descriptor?")
      fileDescriptor <- ProtoParser.findDescriptorProto("Dummy.proto", fileDescriptorSet.getFileList.asScala.toList)
        .fold(IO.raiseError[FileDescriptorProto](new Exception("descriptor not found")))(IO.delay(_))
      protoProtocol <- IO(ProtoParser.fromDescriptor(fileDescriptor))
      _ <- OIO.println(protoProtocol)
      schemaProto <- IO(protocol.fromProtobufProtocol(protoProtocol))
      _ <- OIO.println(schemaProto)
    } yield ExitCode.Success
  }
}
