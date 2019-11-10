package io.higherkindness.ersatz

import higherkindness.droste.{Basis, scheme}
import io.higherkindness.ersatz.Transform.transformProto
import io.higherkindness.ersatz.proto.ProtobufF
import io.higherkindness.ersatz.proto.protocol.{Protocol => ProtoProtocol}

final case class protocol[T](name: String, declarations: List[T])

object protocol {
  def fromProtobufProtocol[T, U](protocol: ProtoProtocol[T])(implicit T: Basis[ProtobufF, T], U: Basis[SchemaF, U]): protocol[U] = {
    val toSchemaF: T => U = scheme.cata(transformProto[U].algebra)

    new protocol[U](
      name = protocol.name,
      declarations = protocol.messages.map(toSchemaF),
    )
  }
}