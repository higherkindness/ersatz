package io.higherkindness.ersatz

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.Applicative
import cats.Traverse
import cats.Functor
import cats.Monad
import cats.instances.list._
import cats.instances.either._
import cats.syntax.all._

import com.google.protobuf.DescriptorProtos.DescriptorProto
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto
import com.google.protobuf.GeneratedMessageV3

import java.nio.file.Paths

import scala.collection.JavaConverters._

import higherkindness.droste.util.DefaultTraverse
import higherkindness.droste.data.Attr
import higherkindness.droste.data.AttrF

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- OIO.println(" ~ ersatz ~ ")

      fileDescriptorSet <- Protoc.descriptor(
        Paths.get("Dummy.proto"),
        Paths.get(".")
      )

      res = fileDescriptorSet.getFileList.asScala.toList.map(
        fileDescriptor => fileDescriptor.getMessageTypeList.asScala.toList.map(message =>
          d2s(message)))

      _ <- res.traverse_(_.traverse_(OIO.println))
    } yield ExitCode.Success

  type Fix[F[_]]
  object Fix {
    def fix[F[_]](f: F[Fix[F]]): Fix[F] = f.asInstanceOf[Fix[F]]
    def unfix[F[_]](ff: Fix[F]): F[Fix[F]] = ff.asInstanceOf[F[Fix[F]]]
  }

  val d2s: GeneratedMessageV3 => Either[String, Fix[SchemaF]] =
    hyloM[Either[String, ?], SchemaF, GeneratedMessageV3, Fix[SchemaF]](
      f => Fix.fix(f).pure[Either[String, ?]],
      Proto2Schema.coalgebra
    )

  def hylo[F[_]: Functor, A, B](
    algebra: F[B] => B,
    coalgebra: A => F[A]
  ): A => B = new (A => B) { self =>
    def apply(a: A): B = {
      val fa: F[A] = coalgebra(a)
      val fb: F[B] = fa.map(self)
      val  b:   B  = algebra(fb)
      b
    }
  }

  def hyloM[M[_]: Monad, F[_]: Traverse, A, B](
    algebraM: F[B] => M[B],
    coalgebraM: A => M[F[A]]
  ): A => M[B] = new (A => M[B]) { self =>
    def apply(a: A): M[B] = for {
      fa: F[A] <- coalgebraM(a)
      fb: F[B] <- fa.traverse(self)
       b:   B  <- algebraM(fb)
    } yield b
  }

}

import scala.reflect.api.Universe
import scala.reflect.runtime.{universe => runtimeUniverse}

object RuntimeADTParser extends ADTParser {
  type UniverseType = runtimeUniverse.type
  val universe: UniverseType = runtimeUniverse
}

sealed trait ADTParser {
  type UniverseType <: Universe
  val universe: UniverseType
}

object Proto2Schema {
  // field.getNumber, field.asRight

    val coalgebra: GeneratedMessageV3 => Either[String, SchemaF[GeneratedMessageV3]] =
      _ match {
        case v: DescriptorProto =>
          Right(RootSchemaF(v.getName, v.getFieldList.asScala.toList))

        case v: FieldDescriptorProto =>
          import FieldDescriptorProto.Type._
          v.getType match {
            case TYPE_STRING => Right(PrimitiveSchemaF(v.getName, PrimitiveType.String))
            case TYPE_BOOL => Right(PrimitiveSchemaF(v.getName, PrimitiveType.Boolean))
            case TYPE_MESSAGE => Right(NestedSchemaF(v.getName, v.getTypeName))
            case todo => Left(s"Unhandled primitive type $todo")
          }

        case other => Left(s"Unable to handle ${other}")
      }
}

sealed trait SchemaF[A] {
  def widen: SchemaF[A] = this
}

sealed trait LeafSchemaF[A] extends SchemaF[A] {
  def retag[B]: SchemaF[B] = this.asInstanceOf[SchemaF[B]]
}

object SchemaF {
  implicit val traverseSchemaaF: Traverse[SchemaF] = new DefaultTraverse[SchemaF] {
    override def traverse[G[_]: Applicative, A, B](fa: SchemaF[A])(f: A => G[B]): G[SchemaF[B]] =
      fa match {
        case RootSchemaF(name, fields) => fields.traverse(f) map { RootSchemaF(name, _) }
        case v: LeafSchemaF[_] => v.retag.pure[G]
      }
  }
}

final case class NestedSchemaF[A](name: String, ref: String) extends LeafSchemaF[A]

final case class RootSchemaF[A](name: String, fields: List[A]) extends SchemaF[A]
object RootSchemaF {
  def apply[A](name: String, fields: List[A]): SchemaF[A] =
    new RootSchemaF(name, fields)
}

final case class PrimitiveSchemaF[A](name: String, primitiveType: PrimitiveType) extends LeafSchemaF[A]

object PrimitiveSchemaF {
  def apply[A](name: String, primitiveType: PrimitiveType): SchemaF[A] =
    new PrimitiveSchemaF(name, primitiveType)
}

sealed trait PrimitiveType
object PrimitiveType {
  final case object String extends PrimitiveType
  final case object Boolean extends PrimitiveType
}
