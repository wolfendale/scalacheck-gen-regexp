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
    version := "1.0.0",
    crossScalaVersions := Seq("2.12.16", "2.13.8", "3.1.0"),
    scalacOptions ++= Seq(
      "-Xfatal-warnings"
    ),
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.17.0" % "provided",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
      "org.scalactic" %% "scalactic" % "3.2.13" % "test",
      "org.scalatest" %% "scalatest" % "3.2.13" % "test",
      "org.scalatestplus" %% "scalacheck-1-16" % "3.2.13.0" % "test"
    )
  )

