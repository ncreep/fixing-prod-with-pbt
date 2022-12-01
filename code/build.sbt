name := "Fixing up production with property-based testing"

version := "0.1"

scalaVersion := "2.13.4"
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

scalacOptions ++= List(
  "-Werror",
  "-deprecation"
)

libraryDependencies ++= List(
  "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
  "org.scalatest" %% "scalatest" % "3.2.14" % Test,
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0" % Test,
)

val zioVersion = "2.0.4"
libraryDependencies ++= List(
  "dev.zio" %% "zio" % zioVersion,
  // didn't end up using zio-test because at some point shrinking
  // was not producing nice enough results
  // but it's quite possible to use here instead of Scalacheck
  "dev.zio" %% "zio-test" % zioVersion % Test,
  "dev.zio" %% "zio-test-magnolia" % zioVersion,
)

libraryDependencies += "com.lihaoyi" %% "pprint" % "0.7.0"

