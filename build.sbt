organization := "juanjovazquez"

name := "s99" 

version := "0.0.1"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-target:jvm-1.7", "-encoding", "utf8", "-feature")

javacOptions += "-g:none"

libraryDependencies ++= Seq(
  "org.scalatest"   %% "scalatest"  % "2.2.4"  % "test",
  "org.scalacheck"  %% "scalacheck" % "1.12.4" % "test"
)