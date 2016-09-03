val commonSettings = Seq(
  scalaVersion := "2.11.8",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

lazy val core = project
  .settings(commonSettings:_*)
 
lazy val server = project
  .settings(commonSettings:_*)
  .settings(libraryDependencies += "com.github.fomkin" %% "korolev-server" % "0.0.2-PRE")

lazy val root = project.in(file("."))
  .aggregate(core, server)
