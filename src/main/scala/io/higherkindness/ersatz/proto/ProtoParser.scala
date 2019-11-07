package io.higherkindness.ersatz.proto

import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.{Label, Type}
import com.google.protobuf.DescriptorProtos.{DescriptorProto, FieldDescriptorProto, FileDescriptorProto}
import higherkindness.droste.{Coalgebra, Embed, scheme}
import io.higherkindness.ersatz.proto.ProtobufF.{TBool, TBytes, TDouble, TFixed32, TFixed64, TFloat, TInt32, TInt64, TNamedType, TNull, TString}
import scala.collection.JavaConverters._

final case class Protocol[T](messages: List[T])

object ProtoParser {
  def fromDescriptor[A](descriptor: FileDescriptorProto)(implicit A: Embed[ProtobufF, A]): Protocol[A] = {
    val named = namedMessages(descriptor)
    descriptor.getMessageTypeList.asScala.toList.map(m =>
      ProtobufF.message[A](m.getName, m.getFieldList.asScala.)
    )
  }

  def fromFieldDescriptorProto[A](
                                   field: FieldDescriptorProto,
                                   source: DescriptorProto,
                                   namedMessages: List[NamedMessage]
                                 )(implicit A: Embed[ProtobufF, A]): FieldF[A] = {
    FieldF.Field(
      field.getName,
      fromFieldType(field, namedMessages),
      field.getNumber,
      field.getLabel.isRepeated
    )
  }

  def fromFieldType[A](field: FieldDescriptorProto, namedMessages: List[NamedMessage])(
    implicit A: Embed[ProtobufF, A]): A =
    scheme.ana(fromFieldTypeCoalgebra(field, namedMessages)).apply(field.getType)

  case class NamedMessage(fullName: String, msg: DescriptorProto)

  def namedMessages(f: FileDescriptorProto): List[NamedMessage] = {
    f.getMessageTypeList.asScala.toList.flatMap(m =>
      NamedMessage(s".${f.getPackage}.${m.getName}", m) :: m.getNestedTypeList.asScala.toList.map(n =>
        NamedMessage(s".${f.getPackage}.${m.getName}.${n.getName}", n)))
  }

  def fromFieldTypeCoalgebra(
                              field: FieldDescriptorProto,
                              namedMessages: List[NamedMessage]
                            ): Coalgebra[ProtobufF, Type] = Coalgebra {
    case Type.TYPE_BOOL => TBool()
    case Type.TYPE_BYTES => TBytes()
    case Type.TYPE_DOUBLE => TDouble()
    case Type.TYPE_FIXED32 => TFixed32()
    case Type.TYPE_FIXED64 => TFixed64()
    case Type.TYPE_FLOAT => TFloat()
    case Type.TYPE_INT32 => TInt32()
    case Type.TYPE_INT64 => TInt64()
    case Type.TYPE_SFIXED32 => TFixed32()
    case Type.TYPE_SFIXED64 => TFixed64()
    case Type.TYPE_SINT32 => TInt32()
    case Type.TYPE_SINT64 => TInt64()
    case Type.TYPE_STRING => TString()
    case Type.TYPE_UINT32 => TInt32()
    case Type.TYPE_UINT64 => TInt64()
    case Type.TYPE_ENUM => ???
    case Type.TYPE_MESSAGE =>
      findMessage(field.getTypeName, namedMessages)
        .fold[ProtobufF[Type]](TNull())(e => TNamedType(e.getName))
    case _ => TNull()
  }

  def findMessage(name: String, namedMessages: List[NamedMessage]): Option[DescriptorProto] = {
    namedMessages.find(_.fullName == name).map(_.msg)
  }

  implicit class LabelOps(self: Label) {
    def isRepeated: Boolean = self.name() == "LABEL_REPEATED"
  }

}
