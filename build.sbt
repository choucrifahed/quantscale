organization := "org.qslib"

name := "quantscale"

version := "0.1.0"

scalaVersion := "2.10.3"

scalacOptions <++= scalaVersion map { v =>
  if (v.startsWith("2.10"))
    Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")
  else
    Seq("-unchecked", "-deprecation")
}

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.2.3",
  "com.typesafe.akka" %% "akka-agent" % "2.2.3",
  "org.scala-saddle" %% "saddle-core" % "1.3.+",
  "org.scalanlp" %% "breeze" % "0.5.2",
  "com.github.nscala-time" %% "nscala-time" % "0.6.0",
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.1" % "test",
  "junit" % "junit" % "4.11" % "test"
)

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)
