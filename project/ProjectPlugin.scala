import sbt.Keys._
import sbt._

object ProjectPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements
  override def requires: Plugins      = plugins.JvmPlugin

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      organization := "io.higherkindness",
      name := "ersatz",
      startYear := Option(2019),
      parallelExecution in Test := false,
      outputStrategy := Some(StdoutOutput),
      connectInput in run := true,
      cancelable in Global := true,
      scalaVersion := "2.12.8",
      turbo := true
    )

}
