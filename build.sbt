enablePlugins(ScalaJSPlugin)

name := "DUX"

version := "0.0.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "hu.thsoft" %%% "firebase-model" % "0.0.0",
  "com.github.japgolly.scalajs-react" %%% "core" % "0.10.4",
  "com.github.japgolly.scalacss" %%% "core" % "0.4.0",
  "com.github.japgolly.scalacss" %%% "ext-react" % "0.4.0"
)

jsDependencies ++= Seq(
  "org.webjars.bower" % "react" % "0.14.3"
    /        "react-with-addons.js"
    minified "react-with-addons.min.js"
    commonJSName "React",
  "org.webjars.bower" % "react" % "0.14.3"
    /         "react-dom.js"
    minified  "react-dom.min.js"
    dependsOn "react-with-addons.js"
    commonJSName "ReactDOM"
)

EclipseKeys.withSource := true