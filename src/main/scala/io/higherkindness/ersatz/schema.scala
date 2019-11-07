package io.higherkindness.ersatz

import cats.Eq
import cats.data.NonEmptyList
import cats.instances.list._
import cats.instances.string._
import cats.instances.option._
import cats.syntax.eq._

sealed trait SchemaF[A]

object SchemaF {
  final case class Field[A](name: String, tpe: A)

  final case class TNull[A]()                                        extends SchemaF[A]
  final case class TDouble[A]()                                      extends SchemaF[A]
  final case class TFloat[A]()                                       extends SchemaF[A]
  final case class TInt[A]()                                         extends SchemaF[A]
  final case class TLong[A]()                                        extends SchemaF[A]
  final case class TBoolean[A]()                                     extends SchemaF[A]
  final case class TString[A]()                                      extends SchemaF[A]
  final case class TByteArray[A]()                                   extends SchemaF[A]
  final case class TNamedType[A](name: String)                       extends SchemaF[A]
  final case class TOption[A](value: A)                              extends SchemaF[A]
  final case class TEither[A](left: A, right: A)                     extends SchemaF[A]
  final case class TList[A](value: A)                                extends SchemaF[A]
  final case class TMap[A](keyTpe: Option[A], value: A)              extends SchemaF[A]
  final case class TGeneric[A](generic: A, params: List[A])          extends SchemaF[A]
  final case class TContaining[A](values: List[A])                   extends SchemaF[A]
  final case class TRequired[A](value: A)                            extends SchemaF[A]
  final case class TCoproduct[A](invariants: NonEmptyList[A])        extends SchemaF[A]
  final case class TSum[A](name: String, fields: List[String])       extends SchemaF[A]
  final case class TProduct[A](name: String, fields: List[Field[A]]) extends SchemaF[A]

  implicit def fieldEq[T](implicit T: Eq[T]): Eq[Field[T]] = Eq.instance {
    case (Field(n, t), Field(n2, t2)) => n === n2 && t === t2
  }

  implicit def muEq[T](implicit T: Eq[T]): Eq[SchemaF[T]] = Eq.instance {
    case (TNull(), TNull())           => true
    case (TDouble(), TDouble())       => true
    case (TFloat(), TFloat())         => true
    case (TInt(), TInt())             => true
    case (TLong(), TLong())           => true
    case (TBoolean(), TBoolean())     => true
    case (TString(), TString())       => true
    case (TByteArray(), TByteArray()) => true

    case (TNamedType(a), TNamedType(b)) => a === b
    case (TOption(a), TOption(b))       => a === b
    case (TList(a), TList(b))           => a === b
    case (TMap(k1, a), TMap(k2, b))     => k1 === k2 && a === b
    case (TRequired(a), TRequired(b))   => a === b

    case (TContaining(a), TContaining(b))   => a === b
    case (TEither(l, r), TEither(l2, r2))   => l === l2 && r === r2
    case (TGeneric(g, p), TGeneric(g2, p2)) => g === g2 && p === p2
    case (TCoproduct(i), TCoproduct(i2))    => i === i2
    case (TSum(n, f), TSum(n2, f2))         => n === n2 && f === f2
    case (TProduct(n, f), TProduct(n2, f2)) => n === n2 && f === f2

    case _ => false
  }


  // smart constructors, to avoid scala inferring specific types instead of SchemaF
  def `null`[A](): SchemaF[A]                                      = TNull()
  def double[A](): SchemaF[A]                                      = TDouble()
  def float[A](): SchemaF[A]                                       = TFloat()
  def int[A](): SchemaF[A]                                         = TInt()
  def long[A](): SchemaF[A]                                        = TLong()
  def boolean[A](): SchemaF[A]                                     = TBoolean()
  def string[A](): SchemaF[A]                                      = TString()
  def byteArray[A](): SchemaF[A]                                   = TByteArray()
  def namedType[A](name: String): SchemaF[A]                       = TNamedType(name)
  def option[A](value: A): SchemaF[A]                              = TOption(value)
  def either[A](left: A, right: A): SchemaF[A]                     = TEither(left, right)
  def list[A](value: A): SchemaF[A]                                = TList(value)
  def map[A](maybeKey: Option[A], value: A): SchemaF[A]            = TMap(maybeKey, value)
  def generic[A](generic: A, params: List[A]): SchemaF[A]          = TGeneric(generic, params)
  def required[A](value: A): SchemaF[A]                            = TRequired(value)
  def coproduct[A](invariants: NonEmptyList[A]): SchemaF[A]        = TCoproduct(invariants)
  def sum[A](name: String, fields: List[String]): SchemaF[A]       = TSum(name, fields)
  def product[A](name: String, fields: List[Field[A]]): SchemaF[A] = TProduct(name, fields)
}
