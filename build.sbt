val commonSettings = Seq(
  scalaVersion := "2.11.8",
  version := "0.1.0",
  maintainer := "Aleksey Fomkin <aleksey.fomkin@gmail.com>",
  packageSummary := "Server for Wizards of Portal game",
  packageDescription := "Server for Wizards of Portal game",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

lazy val core = project
  .settings(commonSettings:_*)
 
lazy val server = project
  .enablePlugins(JavaServerAppPackaging)
  .enablePlugins(LinuxPlugin)
  .enablePlugins(DebianPlugin)
  .enablePlugins(JDebPackaging)
  .enablePlugins(SystemVPlugin)
  .settings(commonSettings:_*)
  .settings(
    name := "Wizards of Portal",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.4.9",
      "com.typesafe.akka" %% "akka-slf4j" % "2.4.9",
      "com.github.fomkin" %% "korolev-server-blaze" % "0.1.0",
      "ch.qos.logback" % "logback-classic" % "1.1.7"
    )
  )
  .dependsOn(core)

lazy val root = project.in(file("."))
  .aggregate(core, server)
