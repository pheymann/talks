
val reactV = "16.2.0"

lazy val common = Seq(
  version      := "-",
  libraryDependencies ++= Seq(
    "com.github.japgolly.scalajs-react" %%% "core" % "1.2.3",
    "org.scala-js" %%% "scalajs-dom" % "0.9.2"
  ),
  jsDependencies ++= Seq(
    "org.webjars.bower" % "react" % "15.2.1" / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
    "org.webjars.bower" % "react" % "15.2.1" / "react-dom.js"         minified "react-dom.min.js" dependsOn "react-with-addons.js" commonJSName "ReactDOM"
  ),

  scalaJSUseMainModuleInitializer := true
)

val copyFast = taskKey[Unit]("Copy fast optimized JS presentation.")

def copyFastImpl(project: String) = Seq(
  copyFast := {
    IO.copyFile(
      target.value / "scala-2.12" / s"$project-fastopt.js",
      baseDirectory.value / "presentation.js"
    )
    IO.copyFile(
      target.value / "scala-2.12" / s"$project-jsdeps.js",
      baseDirectory.value / "jsdeps.js"
    )
  }
)

val copyFull = taskKey[Unit]("Copy fully optimized JS presentation.")

def copyFullImpl(project: String) = Seq(
  copyFull := {
    IO.copyFile(
      target.value / "scala-2.12" / s"$project-opt.js",
      baseDirectory.value / "presentation.js"
    )
    IO.copyFile(
      target.value / "scala-2.12" / s"$project-jsdeps.min.js",
      baseDirectory.value / "jsdeps.js"
    )
  }
)

lazy val root = project
  .in(file("."))
  .aggregate(
    `meetup-recursion`,
    shared
  )

lazy val shared = project
  .in(file("shared"))
  .enablePlugins(ScalaJSPlugin)
  .settings(common)

lazy val `meetup-recursion` = project
  .in(file("meetup-recursion"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    common,
    copyFastImpl("meetup-recursion"),
    copyFullImpl("meetup-recursion"),
    addCommandAlias("fastCompile", "; fastOptJS; copyFast"),
    addCommandAlias("fullCompile", "; fullOptJS; copyFull")
  )
  .dependsOn(shared)

lazy val `scalaio-2018` = project
 .in(file("scalaio-2018"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    common,
    copyFastImpl("scalaio-2018"),
    copyFullImpl("scalaio-2018"),
    addCommandAlias("fastCompile", "; fastOptJS; copyFast"),
    addCommandAlias("fullCompile", "; fullOptJS; copyFull")
  )
  .dependsOn(shared)
