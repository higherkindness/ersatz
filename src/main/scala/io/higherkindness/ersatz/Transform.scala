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
    case ProtobufF.TDouble() => TDouble()
    case ProtobufF.TFloat() => TFloat()
    case ProtobufF.TInt32() => TInt()
    case ProtobufF.TInt64() => TLong()
    case ProtobufF.TUint32() => TInt()
    case ProtobufF.TUint64() => TLong()
    case ProtobufF.TSint32() => TInt()
    case ProtobufF.TSint64() => TLong()
    case ProtobufF.TFixed32() => TInt()
    case ProtobufF.TFixed64() => TLong()
    case ProtobufF.TSfixed32() => TInt()
    case ProtobufF.TSfixed64() => TLong()
    case ProtobufF.TBool() => TBoolean()
    case ProtobufF.TString() => TString()
    case ProtobufF.TBytes() => TByteArray()
    case ProtobufF.TNamedType(name) => TOption(A.algebra(TNamedType(name)))
    case ProtobufF.TRepeated(value) => TList(value)
    case ProtobufF.TMessage(name, fields) => TProduct(name, fields.map(f => Field(f.name, f.tpe)))
  }
}
