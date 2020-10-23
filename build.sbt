name := "makak-scraper"

scalaVersion := "2.13.3"

libraryDependencies ++= {

  object V {
    val scalatest = "3.2.0"
    val cats = "2.2.0"
  }

  Seq(

    // prod
    "org.typelevel" %% "cats-core" % V.cats,
    "org.typelevel" %% "cats-effect" % V.cats,
    "com.iheart" %% "ficus" % "1.5.0",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "net.ruippeixotog" %% "scala-scraper" % "2.2.0",
    "org.apache.commons" % "commons-math3" % "3.6.1",

    // test
    "org.scalatest" %% "scalatest" % V.scalatest % "test"

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
