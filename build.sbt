name := "cameoXliffTools"

version := "0.1"

scalaVersion := "2.11.2"

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)


libraryDependencies ++= Seq (
  "com.typesafe.play" %% "play-json" % "2.3.5",
  "org.clapper" %% "argot" % "1.0.3",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.2"
)
