lazy val root:Project= (project in file(".")).
settings(
  name:="jsbase",
  version:="0.1-SNAPSHOT",
  scalaVersion:="2.12.10",
  scalacOptions ++= Seq( "-deprecation"),
  scalaJSStage in Global := FastOptStage
).enablePlugins(ScalaJSPlugin)
libraryDependencies +=  "dbdef" %%% "dbdef" % "0.9-SNAPSHOT"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.8"


