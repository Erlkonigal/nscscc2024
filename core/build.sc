// import Mill dependency
import mill._
import mill.scalalib._
import mill.scalalib.scalafmt.ScalafmtModule
// support BSP
import mill.bsp._

object core extends ScalaModule with ScalafmtModule { m =>
  override def scalaVersion = "2.13.12"
  override def scalacOptions = Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit"
  )
  override def ivyDeps = Agg(
    ivy"org.chipsalliance::chisel:6.2.0"
  )
  override def scalacPluginIvyDeps = Agg(
    ivy"org.chipsalliance:::chisel-plugin:6.2.0"
  )
}