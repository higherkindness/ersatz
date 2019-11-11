package io.higherkindness.ersatz.proto

import cats.implicits._
import cats.Applicative
import higherkindness.droste.util.DefaultTraverse

sealed trait FieldF[A] {
  val name: String
  val tpe: A
}

object FieldF {
  final case class Field[A](
                             name: String,
                             tpe: A,
                             position: Int,
                             isRepeated: Boolean)
    extends FieldF[A]

  final case class OneOfField[A](name: String, tpe: A) extends FieldF[A]
}

sealed trait ProtobufF[A]

object ProtobufF {

  final case class TNull[A]() extends ProtobufF[A]

  final case class TUint64[A]() extends ProtobufF[A]

  final case class TBool[A]() extends ProtobufF[A]

  final case class TString[A]() extends ProtobufF[A]

  final case class TNamedType[A](name: String) extends ProtobufF[A]

  final case class TRepeated[A](value: A) extends ProtobufF[A]

  final case class TMessage[A](name: String, fields: List[FieldF[A]]) extends ProtobufF[A]

  def uint64[A](): ProtobufF[A] = TUint64()

  def bool[A](): ProtobufF[A] = TBool()

  def string[A](): ProtobufF[A] = TString()

  def namedType[A](name: String): ProtobufF[A] = TNamedType(name)

  def repeated[A](value: A): ProtobufF[A] = TRepeated(value)

  def message[A](name: String, fields: List[FieldF[A]]): ProtobufF[A] = TMessage(name, fields)

  implicit val traverse: DefaultTraverse[ProtobufF] = new DefaultTraverse[ProtobufF] {
    def traverse[G[_], A, B](fa: ProtobufF[A])(f: A => G[B])(implicit G: Applicative[G]): G[ProtobufF[B]] = {

      def makeFieldB(field: FieldF.Field[A]): G[FieldF.Field[B]] =
        f(field.tpe).map(b =>
          FieldF.Field[B](field.name, b, field.position, field.isRepeated))

      def traverseFieldF(fieldFList: List[FieldF[A]]): G[List[FieldF[B]]] =
        fieldFList.traverse {
          case field: FieldF.Field[A] => makeFieldB(field).widen
        }

      fa match {
        case TUint64() => uint64[B]().pure[G]
        case TBool() => bool[B]().pure[G]
        case TString() => string[B]().pure[G]
        case TNamedType(name) => namedType[B](name).pure[G]
        case TRepeated(value) => f(value).map(TRepeated[B])
        case TMessage(name, fields) =>
          traverseFieldF(fields).map(bFields => TMessage[B](name, bFields))
      }
    }
  }
}