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