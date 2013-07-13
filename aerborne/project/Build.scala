import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "aerborne"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    "commons-codec" % "commons-codec" % "1.6",
    "org.apache.httpcomponents" % "httpclient" % "4.0.1",
    "org.apache.httpcomponents" % "httpcore" % "4.0.1",
    "commons-logging" % "commons-logging" % "1.1.3"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here
    resolvers ++= Seq(
      Resolver.url("Codahale Repo", url("http://repo.codahale.com"))
    )
  )

}
