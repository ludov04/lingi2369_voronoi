name := "lingi2369_voronoi_2"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies += "com.vividsolutions" % "jts" % "1.13"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test"

unmanagedBase <<= baseDirectory { base => base / "lib" }