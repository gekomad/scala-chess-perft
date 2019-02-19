name := "scala-chess-perft"

version := "0.0.1"
organization := "com.github.gekomad"
scalaVersion := "2.12.8"

libraryDependencies += "org.scalatest" %% "scalatest"  % "3.0.5"  % Test

crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.6", "2.12.8","2.13.0-M5")

scalacOptions in Compile ++= Seq("-Xdisable-assertions")
