name := "scala-tron"

version := "0.0.0"

scalaVersion := "2.10.2"

libraryDependencies <+= scalaVersion {
  "org.scala-lang" % "scala-swing" % _
}

libraryDependencies <+= scalaVersion {
  "org.scala-lang" % "scala-actors" % _
}

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

// End up with : Can't handle methods with more than 9 parameters (yet) when mocking Graphics2D
//libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.0.1" % "test"

libraryDependencies += "org.easymock" % "easymock" % "3.2" % "test"

// Doesn't seem to work idea and the new sbt plugin
//libraryDependencies += "junit" % "junit" % "4.11" % "test"
