
lazy val defaultSettings = Seq(
  scalaVersion := Common.scalaVersion,
  version := "0.1.6",
  scalacOptions := Seq("-encoding", "UTF-8", "-target:jvm-1.8",
    "-deprecation", "-unchecked",
    "-feature", "-Ybreak-cycles",
    "-language:reflectiveCalls",
    "-language:existentials"
  ),
  javacOptions := Seq("-encoding", "UTF-8", "-source", "1.8", "-target", "1.8"),
  parallelExecution in Test := false,
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % Common.scalaVersion,
    "org.scalatest" %% "scalatest" % "3.0.8" % Test
  ),
)

lazy val dataflow = (project in file("."))
  .settings(name := "myproject")
  .aggregate(`practice`)

lazy val `practice` = project.in(file("practice"))
  .settings(defaultSettings: _*)
