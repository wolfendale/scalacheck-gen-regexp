lazy val root = (project in file("."))
  .enablePlugins(BintrayPlugin)
  .settings(
    organization := "wolfendale",
    name := "scalacheck-gen-regexp",
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    homepage := Some(url("https://github.com/wolfendale/scalacheck-gen-regexp")),
    bintrayVcsUrl := Some("https://github.com/wolfendale/scalacheck-gen-regexp"),
    version := "0.1.2",
    crossScalaVersions := Seq("2.11.12", "2.12.10", "2.13.1"),
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.2" % "provided",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "org.scalactic" %% "scalactic" % "3.0.8" % "test",
      "org.scalatest" %% "scalatest" % "3.0.8" % "test"
    )
  )

