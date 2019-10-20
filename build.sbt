lazy val V = new {
  val cats            = "2.0.0"
  val catsEffect      = "2.0.0"
  val droste          = "0.7.0"
  val fs2             = "2.0.1"
  val protobuf        = "3.10.0"
  val flatbuffers     = "1.11.1"
  val avro            = "1.9.1"
  val circe           = "0.12.2"
  val circeDerivation = "0.12.0-M7"
  val protoc          = "2.4.1"
  val scalacheck      = "1.14.0"
}

libraryDependencies ++= Seq(
  "org.typelevel"          %% "cats-core" % V.cats,
  "org.typelevel"          %% "cats-free" % V.cats,
  "org.typelevel"          %% "cats-effect" % V.catsEffect,
  "co.fs2"                 %% "fs2-core" % V.fs2,
  "com.google.protobuf"     % "protobuf-java" % V.protobuf,
  "com.google.flatbuffers"  % "flatbuffers-java" % V.flatbuffers,
  "org.apache.avro"         % "avro" % V.avro,
  "io.circe"               %% "circe-core" % V.circe,
  "io.circe"               %% "circe-derivation" % V.circeDerivation,
  "com.github.os72"         % "protoc" % V.protoc,
  "io.higherkindness"      %% "droste-core" % V.droste,
)
