enablePlugins(ScalaJSPlugin)
lazy val root:Project= (project in file(".")).
settings(
  name:="jsbase",
  version:="0.1-SNAPSHOT",
  scalaVersion:="2.12.1",
  scalacOptions ++= Seq( "-deprecation"),
  scalaJSStage in Global := FastOptStage
)
libraryDependencies +=  "dbdef" %%% "dbdef" % "0.9-SNAPSHOT"
//libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.6"
//libraryDependencies += "org.scala-js" %% "scalajs-javalib-ex" % "0.6.13"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.2"


