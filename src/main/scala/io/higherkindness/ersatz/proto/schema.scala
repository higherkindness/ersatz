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

  final case class TDouble[A]() extends ProtobufF[A]

  final case class TFloat[A]() extends ProtobufF[A]

  final case class TInt32[A]() extends ProtobufF[A]

  final case class TInt64[A]() extends ProtobufF[A]

  final case class TUint32[A]() extends ProtobufF[A]

  final case class TUint64[A]() extends ProtobufF[A]

  final case class TSint32[A]() extends ProtobufF[A]

  final case class TSint64[A]() extends ProtobufF[A]

  final case class TFixed32[A]() extends ProtobufF[A]

  final case class TFixed64[A]() extends ProtobufF[A]

  final case class TSfixed32[A]() extends ProtobufF[A]

  final case class TSfixed64[A]() extends ProtobufF[A]

  final case class TBool[A]() extends ProtobufF[A]

  final case class TString[A]() extends ProtobufF[A]

  final case class TBytes[A]() extends ProtobufF[A]

  final case class TNamedType[A](name: String) extends ProtobufF[A]

  final case class TRepeated[A](value: A) extends ProtobufF[A]

  final case class TMessage[A](name: String, fields: List[FieldF[A]]) extends ProtobufF[A]

  def `null`[A](): ProtobufF[A] = TNull()

  def double[A](): ProtobufF[A] = TDouble()

  def float[A](): ProtobufF[A] = TFloat()

  def int32[A](): ProtobufF[A] = TInt32()

  def int64[A](): ProtobufF[A] = TInt64()

  def uint32[A](): ProtobufF[A] = TUint32()

  def uint64[A](): ProtobufF[A] = TUint64()

  def sint32[A](): ProtobufF[A] = TSint32()

  def sint64[A](): ProtobufF[A] = TSint64()

  def fixed32[A](): ProtobufF[A] = TFixed32()

  def fixed64[A](): ProtobufF[A] = TFixed64()

  def sfixed32[A](): ProtobufF[A] = TSfixed32()

  def sfixed64[A](): ProtobufF[A] = TSfixed64()

  def bool[A](): ProtobufF[A] = TBool()

  def string[A](): ProtobufF[A] = TString()

  def bytes[A](): ProtobufF[A] = TBytes()

  def namedType[A](name: String): ProtobufF[A] = TNamedType(name)

  def repeated[A](value: A): ProtobufF[A] = TRepeated(value)

  def message[A](name: String, fields: List[FieldF[A]]): ProtobufF[A] = TMessage(name, fields)

  implicit def protobufEq[T: Eq]: Eq[ProtobufF[T]] = Eq.instance {
    case (TNull(), TNull()) => true
    case (TDouble(), TDouble()) => true
    case (TFloat(), TFloat()) => true
    case (TInt32(), TInt32()) => true
    case (TInt64(), TInt64()) => true
    case (TUint32(), TUint32()) => true
    case (TUint64(), TUint64()) => true
    case (TSint32(), TSint32()) => true
    case (TSint64(), TSint64()) => true
    case (TFixed32(), TFixed32()) => true
    case (TFixed64(), TFixed64()) => true
    case (TSfixed32(), TSfixed32()) => true
    case (TSfixed64(), TSfixed64()) => true
    case (TBool(), TBool()) => true
    case (TString(), TString()) => true
    case (TBytes(), TBytes()) => true
    case (TNamedType(n), TNamedType(n2)) => n === n2
    case (TRepeated(v), TRepeated(v2)) => v === v2
    case (TMessage(n, f), TMessage(n2, f2)) => n === n2 && f === f2
    case _ => false
  }

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
        case TNull() => `null`[B]().pure[G]
        case TDouble() => double[B]().pure[G]
        case TFloat() => float[B]().pure[G]
        case TInt32() => int32[B]().pure[G]
        case TInt64() => int64[B]().pure[G]
        case TUint32() => uint32[B]().pure[G]
        case TUint64() => uint64[B]().pure[G]
        case TSint32() => sint32[B]().pure[G]
        case TSint64() => sint64[B]().pure[G]
        case TFixed32() => fixed32[B]().pure[G]
        case TFixed64() => fixed64[B]().pure[G]
        case TSfixed32() => sfixed32[B]().pure[G]
        case TSfixed64() => sfixed64[B]().pure[G]
        case TBool() => bool[B]().pure[G]
        case TString() => string[B]().pure[G]
        case TBytes() => bytes[B]().pure[G]
        case TNamedType(name) => namedType[B](name).pure[G]
        case TRepeated(value) => f(value).map(TRepeated[B])
        case TMessage(name, fields) =>
          traverseFieldF(fields).map(bFields => TMessage[B](name, bFields))
      }
    }
  }

}
