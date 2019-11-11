package io.higherkindness.ersatz

sealed trait SchemaF[A]

object SchemaF {
  final case class Field[A](name: String, tpe: A)

  final case class TNull[A]()                                        extends SchemaF[A]
  final case class TLong[A]()                                        extends SchemaF[A]
  final case class TBoolean[A]()                                     extends SchemaF[A]
  final case class TString[A]()                                      extends SchemaF[A]
  final case class TNamedType[A](name: String)                       extends SchemaF[A]
  final case class TOption[A](value: A)                              extends SchemaF[A]
  final case class TList[A](value: A)                                extends SchemaF[A]
  final case class TRequired[A](value: A)                            extends SchemaF[A]
  final case class TProduct[A](name: String, fields: List[Field[A]]) extends SchemaF[A]
}