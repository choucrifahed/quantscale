organization := "org.qslib"

name := "quantscale"

version := "0.1.0"

scalaVersion := "2.10.1"

scalacOptions <++= scalaVersion map { v =>
  if (v.startsWith("2.10"))
    Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")
  else
    Seq("-unchecked", "-deprecation")
}

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.1.2",
  "com.typesafe.akka" %% "akka-agent" % "2.1.2",
  "org.scala-saddle" %% "saddle" % "1.1.0",
  "org.scalanlp" %% "breeze-learn" % "0.2",
  "org.scalaj" % "scalaj-time_2.9.2" % "0.6",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
  "junit" % "junit" % "4.11" % "test"
)

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)
