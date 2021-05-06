import xerial.sbt.Sonatype.GitHubHosting

lazy val root = (project in file("."))
  .settings(
    organization := "io.github.wolfendale",
    name := "scalacheck-gen-regexp",
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    homepage := Some(url("https://github.com/wolfendale/scalacheck-gen-regexp")),
    sonatypeProjectHosting := Some(GitHubHosting("wolfendale", "scalacheck-gen-regexp", "michael.wolfendale@gmail.com")),
    publishMavenStyle := true,
    publishTo := sonatypePublishToBundle.value,
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    version := "0.1.3",
    crossScalaVersions := Seq("2.11.12", "2.12.10", "2.13.5"),
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.2" % "provided",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "org.scalactic" %% "scalactic" % "3.0.8" % "test",
      "org.scalatest" %% "scalatest" % "3.0.8" % "test"
    )
  )

