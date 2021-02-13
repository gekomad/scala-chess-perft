name := "scala-chess-perft"

version := "0.0.3"
organization := "com.github.gekomad"
scalaVersion := "2.13.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.3.0-SNAP3" % Test

crossScalaVersions := Seq("2.12.11")

scalacOptions in Compile ++= Seq("-Xdisable-assertions")
scalacOptions ++= Seq(
  "-deprecation",
  "-language:postfixOps",
  "-feature",
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  "-Xlint:option-implicit", // Option.apply used implicit view.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-explaintypes", // Explain type errors in more detail.
  "-Xfatal-warnings"
)
