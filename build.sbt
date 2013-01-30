name := "quantscale"

version := "0.1.0"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-deprecation")

libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze-math" % "0.2-SNAPSHOT",
    "org.scalanlp" %% "breeze-learn" % "0.2-SNAPSHOT",
    "org.scalanlp" %% "breeze-process" % "0.2-SNAPSHOT",
    "org.scalanlp" %% "breeze-viz" % "0.2-SNAPSHOT",
	"org.scalatest" %% "scalatest" % "1.9.1" % "test",
	"junit" % "junit" % "4.11" % "test"
)

resolvers ++= Seq(
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)
