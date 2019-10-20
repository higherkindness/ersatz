package io.higherkindness.ersatz

import cats.Eq
import cats.implicits._

sealed trait FieldF[A] {
  val name: String
  val tpe: A
}

object FieldF {
  final case class Field[A](
                             name: String,
                             tpe: A,
                             position: Int,
                             isRepeated: Boolean,
                             isMapField: Boolean)
    extends FieldF[A]

  final case class OneOfField[A](name: String, tpe: A) extends FieldF[A]

  implicit def fieldEq[T: Eq]: Eq[FieldF[T]] = Eq.instance {
    case (Field(n, t, p, r, m), Field(n2, t2, p2, r2, m2)) =>
      n === n2 && t === t2 && p === p2 && r === r2 && m === m2
    case (OneOfField(n, tpe), OneOfField(n2, tpe2)) =>
      n === n2 && tpe === tpe2
    case _ => false
  }
}

sealed trait ProtobufF[A]

object ProtobufF {

  final case class OptionValue(name: String, value: String)
  object OptionValue {
    implicit val optionEq: Eq[OptionValue] = Eq.instance {
      case (OptionValue(n, v), OptionValue(n2, v2)) => n === n2 && v === v2
    }
  }

  final case class TNull[A]()                                                     extends ProtobufF[A]
  final case class TDouble[A]()                                                   extends ProtobufF[A]
  final case class TFloat[A]()                                                    extends ProtobufF[A]
  final case class TInt32[A]()                                                    extends ProtobufF[A]
  final case class TInt64[A]()                                                    extends ProtobufF[A]
  final case class TUint32[A]()                                                   extends ProtobufF[A]
  final case class TUint64[A]()                                                   extends ProtobufF[A]
  final case class TSint32[A]()                                                   extends ProtobufF[A]
  final case class TSint64[A]()                                                   extends ProtobufF[A]
  final case class TFixed32[A]()                                                  extends ProtobufF[A]
  final case class TFixed64[A]()                                                  extends ProtobufF[A]
  final case class TSfixed32[A]()                                                 extends ProtobufF[A]
  final case class TSfixed64[A]()                                                 extends ProtobufF[A]
  final case class TBool[A]()                                                     extends ProtobufF[A]
  final case class TString[A]()                                                   extends ProtobufF[A]
  final case class TBytes[A]()                                                    extends ProtobufF[A]
  final case class TNamedType[A](name: String)                                    extends ProtobufF[A]
  final case class TRepeated[A](value: A)                                         extends ProtobufF[A]
  final case class TMessage[A](name: String, fields: List[FieldF[A]]) extends ProtobufF[A]

  def `null`[A](): ProtobufF[A]                                                   = TNull()
  def double[A](): ProtobufF[A]                                                   = TDouble()
  def float[A](): ProtobufF[A]                                                    = TFloat()
  def int32[A](): ProtobufF[A]                                                    = TInt32()
  def int64[A](): ProtobufF[A]                                                    = TInt64()
  def uint32[A](): ProtobufF[A]                                                   = TUint32()
  def uint64[A](): ProtobufF[A]                                                   = TUint64()
  def sint32[A](): ProtobufF[A]                                                   = TSint32()
  def sint64[A](): ProtobufF[A]                                                   = TSint64()
  def fixed32[A](): ProtobufF[A]                                                  = TFixed32()
  def fixed64[A](): ProtobufF[A]                                                  = TFixed64()
  def sfixed32[A](): ProtobufF[A]                                                 = TSfixed32()
  def sfixed64[A](): ProtobufF[A]                                                 = TSfixed64()
  def bool[A](): ProtobufF[A]                                                     = TBool()
  def string[A](): ProtobufF[A]                                                   = TString()
  def bytes[A](): ProtobufF[A]                                                    = TBytes()
  def namedType[A](name: String): ProtobufF[A]                                    = TNamedType(name)
  def repeated[A](value: A): ProtobufF[A]                                         = TRepeated(value)

  def message[A](name: String, fields: List[FieldF[A]]): ProtobufF[A] = TMessage(name, fields)

  implicit def protobufEq[T: Eq]: Eq[ProtobufF[T]] = Eq.instance {
    case (TNull(), TNull())                         => true
    case (TDouble(), TDouble())                     => true
    case (TFloat(), TFloat())                       => true
    case (TInt32(), TInt32())                       => true
    case (TInt64(), TInt64())                       => true
    case (TUint32(), TUint32())                     => true
    case (TUint64(), TUint64())                     => true
    case (TSint32(), TSint32())                     => true
    case (TSint64(), TSint64())                     => true
    case (TFixed32(), TFixed32())                   => true
    case (TFixed64(), TFixed64())                   => true
    case (TSfixed32(), TSfixed32())                 => true
    case (TSfixed64(), TSfixed64())                 => true
    case (TBool(), TBool())                         => true
    case (TString(), TString())                     => true
    case (TBytes(), TBytes())                       => true
    case (TNamedType(n), TNamedType(n2))            => n === n2
    case (TRepeated(v), TRepeated(v2))              => v === v2
    case (TMessage(n, f), TMessage(n2, f2))  => n === n2 && f === f2
    case _                                          => false
  }
}
