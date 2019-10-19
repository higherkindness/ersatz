import sbt._
import sbt.Keys._

object ScalacOptionsPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements
  override def requires: Plugins      = plugins.JvmPlugin

  lazy val scalac211Options =
    taskKey[Seq[String]]("Options for the Scala 2.11 compiler.")
  lazy val scalac212Options =
    taskKey[Seq[String]]("Options for the Scala 2.12 compiler.")
  lazy val scalac213Options =
    taskKey[Seq[String]]("Options for the Scala 2.13 compiler.")

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    scalac211Options := defaultScalac211Options,
    scalac212Options := defaultScalac212Options,
    scalac213Options := defaultScalac213Options,
    scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => scalac213Options.value
      case Some((2, 12)) => scalac212Options.value
      case Some((2, 11)) => scalac211Options.value
      case _             => Nil
    }),
    scalacOptions in (Compile, doc) ~= (_.filterNot(scalacOptionsDocsFilter)),
    scalacOptions in (Compile, doc) += "-groups",
    scalacOptions in (Compile, console) ~= (_.filterNot(
      scalacOptionsConsoleFilter)),
    scalacOptions in (Test, console) ~= (_.filterNot(
      scalacOptionsConsoleFilter))
  )

  private[this] def defaultScalac213Options: List[String] =
    defaultScalac212Options.filterNot(
      Set(
        "-Yno-adapted-args",
        "-Ypartial-unification"
      )) ++ List(
      "-Ymacro-annotations"
    )

  private[this] def defaultScalac212Options: List[String] =
    List(
      "-deprecation", // Emit warning and location for usages of deprecated APIs.
      "-encoding",
      "utf-8", // Specify character encoding used by source files.
      "-explaintypes", // Explain type errors in more detail.
      "-feature", // Emit warning and location for usages of features that should be imported explicitly.
      "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
      "-language:experimental.macros", // Allow macro definition (besides implementation and application)
      "-language:higherKinds", // Allow higher-kinded types
      "-language:implicitConversions", // Allow definition of implicit functions called views
      "-unchecked",  // Enable additional warnings where generated code depends on assumptions.
      "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
      //"-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
      "-Xfuture", // Turn on future language features.
      "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
      "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
      "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
      "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
      "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
      "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
      "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
      "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
      "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
      "-Xlint:option-implicit", // Option.apply used implicit view.
      "-Xlint:package-object-classes", // Class or object defined in package object.
      "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
      "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
      "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
      "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
      "-Xlint:unsound-match", // Pattern match may not be typesafe.
      "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
      "-Ypartial-unification", // Enable partial unification in type constructor inference
      "-Ywarn-dead-code", // Warn when dead code is identified.
      "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
      "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
      "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
      "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
      "-Ywarn-numeric-widen", // Warn when numerics are widened.
      "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
      "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
      "-Ywarn-unused:locals", // Warn if a local definition is unused.
      "-Ywarn-unused:params", // Warn if a value parameter is unused.
      "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
      "-Ywarn-unused:privates", // Warn if a private member is unused.
      "-Ywarn-value-discard"
    ) // Warn when non-Unit expression results are unused.

  private[this] def defaultScalac211Options: List[String] =
    List(
      "-encoding",
      "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:experimental.macros",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Ypartial-unification"
    )

  private[this] def scalacOptionsConsoleFilter: Set[String] =
    Set("-Ywarn-unused:imports", "-Ywarn-unused-import", "-Xfatal-warnings")

  private[this] def scalacOptionsDocsFilter: Set[String] =
    Set("-Ywarn-unused:imports", "-Ywarn-unused-import", "-Xfatal-warnings")

}
