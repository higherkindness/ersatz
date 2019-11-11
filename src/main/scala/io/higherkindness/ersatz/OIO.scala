package io.higherkindness.ersatz

import cats.effect.IO
import cats.effect.Resource

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption

object OIO {

  def println(any: Any): IO[Unit] = IO(Console.println(any))

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
