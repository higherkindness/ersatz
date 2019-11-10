package io.higherkindness.ersatz

import cats.Eq
import cats.instances.list._
import cats.instances.string._
import cats.syntax.eq._
import higherkindness.droste.macros.deriveTraverse

@deriveTraverse sealed trait SchemaF[A]

object SchemaF {
  @deriveTraverse final case class Field[A](name: String, tpe: A)

  final case class TNull[A]()                                        extends SchemaF[A]
  final case class TLong[A]()                                        extends SchemaF[A]
  final case class TBoolean[A]()                                     extends SchemaF[A]
  final case class TString[A]()                                      extends SchemaF[A]
  final case class TNamedType[A](name: String)                       extends SchemaF[A]
  final case class TOption[A](value: A)                              extends SchemaF[A]
  final case class TList[A](value: A)                                extends SchemaF[A]
  final case class TRequired[A](value: A)                            extends SchemaF[A]
  final case class TProduct[A](name: String, fields: List[Field[A]]) extends SchemaF[A]

  implicit def fieldEq[T](implicit T: Eq[T]): Eq[Field[T]] = Eq.instance {
    case (Field(n, t), Field(n2, t2)) => n === n2 && t === t2
  }

  implicit def schemaEq[T](implicit T: Eq[T]): Eq[SchemaF[T]] = Eq.instance {
    case (TNull(), TNull())           => true
    case (TLong(), TLong())           => true
    case (TBoolean(), TBoolean())     => true
    case (TString(), TString())       => true

    case (TNamedType(a), TNamedType(b)) => a === b
    case (TOption(a), TOption(b))       => a === b
    case (TList(a), TList(b))           => a === b
    case (TRequired(a), TRequired(b))   => a === b

    case (TProduct(n, f), TProduct(n2, f2)) => n === n2 && f === f2

    case _ => false
  }


  // smart constructors, to avoid scala inferring specific types instead of SchemaF
  def `null`[A](): SchemaF[A]                                      = TNull()
  def long[A](): SchemaF[A]                                        = TLong()
  def boolean[A](): SchemaF[A]                                     = TBoolean()
  def string[A](): SchemaF[A]                                      = TString()
  def namedType[A](name: String): SchemaF[A]                       = TNamedType(name)
  def option[A](value: A): SchemaF[A]                              = TOption(value)
  def list[A](value: A): SchemaF[A]                                = TList(value)
  def required[A](value: A): SchemaF[A]                            = TRequired(value)
  def product[A](name: String, fields: List[Field[A]]): SchemaF[A] = TProduct(name, fields)
}