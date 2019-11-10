package io.higherkindness.ersatz.proto

object protocol {

  final case class Protocol[T](name: String, messages: List[T])

}
