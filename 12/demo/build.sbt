name := "web-demo"

organization := "ru.spbsu"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client3" %% "core" % "3.3.0",  // http client
  "com.bot4s" %% "telegram-core" % "4.4.0-RC2",                // telegram api
  "org.typelevel" %% "cats-effect" % "3.1.0",
  "com.softwaremill.sttp.client3" %% "async-http-client-backend-cats" % "3.3.0", // for cats-effect 3.x
  "com.typesafe.slick" %% "slick" % "3.3.3",            // database api
  "org.mnode.ical4j" % "ical4j" % "4.0.0-alpha9",       // ICAL parser
  "org.slf4j" % "slf4j-nop" % "1.6.4",                  // misc: logger
  "com.h2database" % "h2" % "1.4.200" % Test,
  "org.scalatest" %% "scalatest" % "3.2.7" % Test
)