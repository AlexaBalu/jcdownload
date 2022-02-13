name := "jcdownload"

version := "0.1"

scalaVersion := "2.13.1"

// https://mvnrepository.com/artifact/commons-io/commons-io
libraryDependencies += "commons-io" % "commons-io" % "2.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

//libraryDependencies += "javax.xml.bind" %% "jaxb-api" % "2.3.0"

//libraryDependencies += "com.sun.xml.bind" %% "jaxb-impl" % "2.3.0"

//libraryDependencies += "org.glassfish.jaxb" %% "jaxb-runtime" % "2.3.0"

//libraryDependencies += "javax.activation" %% "activation" % "1.1.1"

libraryDependencies += "com.google.code.gson" % "gson" % "2.8.6"

assemblyJarName in assembly := "jcdownload.jar"

mainClass in assembly := Some("info.cemu.download.Main")

//test in assembly := {}