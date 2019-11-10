package io.higherkindness.ersatz.proto

import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.{Label, Type}
import com.google.protobuf.DescriptorProtos.{DescriptorProto, FieldDescriptorProto, FileDescriptorProto}
import higherkindness.droste.{Coalgebra, Embed, scheme}
import io.higherkindness.ersatz.proto.ProtobufF.{TBool, TNamedType, TNull, TString, TUint64}
import higherkindness.droste.syntax.embed._
import io.higherkindness.ersatz.proto.protocol.Protocol

import scala.collection.JavaConverters._

object ProtoParser {
  def findDescriptorProto(name: String, files: List[FileDescriptorProto]): Option[FileDescriptorProto] =
    files.find(_.getName == name)

  def fromDescriptor[A](descriptor: FileDescriptorProto)(implicit A: Embed[ProtobufF, A]): Protocol[A] = {
    val named = namedMessages(descriptor)
    val messages = descriptor.getMessageTypeList.asScala.toList.map(m =>
      ProtobufF.message[A](m.getName, m.getFieldList.asScala.toList.map(fromFieldDescriptorProto(_, named))).embed
    )

    Protocol[A](descriptor.getName, messages)
  }

  def fromFieldDescriptorProto[A](
                                   field: FieldDescriptorProto,
                                  // source: DescriptorProto,
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
    case Type.TYPE_STRING => TString()
    case Type.TYPE_UINT64 => TUint64()
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
