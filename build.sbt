name := "scalacheck-gen-regexp"

version := "0.1"

crossScalaVersions := Seq("2.11.12", "2.12.4")

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "provided",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.scalactic" %% "scalactic" % "3.0.4" % "test",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)