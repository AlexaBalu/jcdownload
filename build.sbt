name := "jcdownload"

version := "0.1"

scalaVersion := "2.13.1"

// https://mvnrepository.com/artifact/commons-io/commons-io
libraryDependencies += "commons-io" % "commons-io" % "2.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "com.google.code.gson" % "gson" % "2.8.6"

assemblyJarName in assembly := "jcdownload.jar"

//test in assembly := {}