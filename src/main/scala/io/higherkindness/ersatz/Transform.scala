package io.higherkindness.ersatz

import higherkindness.droste.{Embed, Trans}
import io.higherkindness.ersatz.proto.ProtobufF

object Transform {

  import SchemaF._

  /**
   * transform Protobuf schema into SchemaF
   */
  def transformProto[A](implicit A: Embed[SchemaF, A]): Trans[ProtobufF, SchemaF, A] = Trans {
    case ProtobufF.TNull() => TNull()
    case ProtobufF.TUint64() => TLong()
    case ProtobufF.TBool() => TBoolean()
    case ProtobufF.TString() => TString()
    case ProtobufF.TNamedType(name) => TOption(A.algebra(TNamedType(name)))
    case ProtobufF.TRepeated(value) => TList(value)
    case ProtobufF.TMessage(name, fields) => TProduct(name, fields.map(f => Field(f.name, f.tpe)))
  }
}
