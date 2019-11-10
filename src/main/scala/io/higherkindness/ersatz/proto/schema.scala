package io.higherkindness.ersatz.proto

import cats.implicits._
import cats.{Applicative, Eq}
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

  implicit def fieldEq[T: Eq]: Eq[FieldF[T]] = Eq.instance {
    case (Field(n, t, p, r), Field(n2, t2, p2, r2)) =>
      n === n2 && t === t2 && p === p2 && r === r2
    case (OneOfField(n, tpe), OneOfField(n2, tpe2)) =>
      n === n2 && tpe === tpe2
    case _ => false
  }
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

  implicit def protobufEq[T: Eq]: Eq[ProtobufF[T]] = Eq.instance {
    case (TNull(), TNull()) => true
    case (TUint64(), TUint64()) => true
    case (TBool(), TBool()) => true
    case (TString(), TString()) => true
    case (TNamedType(n), TNamedType(n2)) => n === n2
    case (TRepeated(v), TRepeated(v2)) => v === v2
    case (TMessage(n, f), TMessage(n2, f2)) => n === n2 && f === f2
    case _ => false
  }

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
       // case TNull() => `null`[B]().pure[G]
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