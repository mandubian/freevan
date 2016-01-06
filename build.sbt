cancelable in Global := true

lazy val commonSettings =  Seq(
    organization := "freevan"
  , name := "freevan"
  , version := "0.1.0-SNAPSHOT"
  , scalaVersion := "2.11.7"
  , logLevel in update := Level.Warn
)

lazy val strictScalac =
  scalacOptions ++= Seq(
    "-Yrangepos"
    , "-Xlint"
    ,"-deprecation"
    , "-Xfatal-warnings"
    , "-feature"
    , "-encoding", "UTF-8"
    , "-unchecked"
    , "-Yno-adapted-args"
    , "-Ywarn-dead-code"
    , "-Ywarn-numeric-widen"
    , "-Ywarn-value-discard"
    , "-Xfuture"
    , "-Ywarn-unused-import"
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.7.1" cross CrossVersion.binary)

lazy val root = project.in(file("."))
  .settings(commonSettings:_*)
  .settings(name := "scalaeff")
  .settings(
    libraryDependencies ++= Seq(
      // "com.chuusai"     %%  "shapeless"        % "2.2.5"
        "org.spire-math"  %%  "cats"             % "0.3.0"
      , "org.scalatest"   %%  "scalatest"        % "2.2.1"   % "test"
      // , "org.typelevel"   %% "alleycats"         % "0.1.2"
      // , "org.typelevel"   %%  "export-hook"      % "1.1.0"
      // , "org.scala-lang"  % "scala-reflect"      % scalaVersion.value % "provided"
      // , compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
    )
  )
