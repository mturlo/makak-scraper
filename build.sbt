name := "makak-scraper"

scalaVersion := "2.13.3"

libraryDependencies ++= {

  object Versions {
    val scalatest = "3.2.0"
  }

  Seq(

    // prod
    "com.iheart" %% "ficus" % "1.5.0",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
    "ch.qos.logback" % "logback-classic" % "1.2.3",

    // test
    "org.scalatest" %% "scalatest" % Versions.scalatest % "test"

  ).map(_ withSources() withJavadoc())

}

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-Xfatal-warnings",
  "-language:postfixOps"
)

lazy val root = project in file(".")
turbo := true
