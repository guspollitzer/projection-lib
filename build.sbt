name := "projection-lib"

version := "0.1"

scalaVersion := "3.1.3"

idePackagePrefix := Some("wms.flow.planner")

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.2.14" % "test",
	"org.scalacheck" %% "scalacheck" % "1.17.0" % "test"
)

scalacOptions ++= Seq("-deprecation", "-feature")