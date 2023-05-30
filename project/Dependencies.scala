import sbt._

object Dependencies {

  object Cats {
    private val catsVersion   = "2.9.0"
    private val effectVersion = "3.5.0"

    val Core                  = "org.typelevel" %% "cats-core"   % catsVersion
    val Effect                = "org.typelevel" %% "cats-effect" % effectVersion
  }

  object Circe {
    private val circeVersion = "0.14.5"

    val CirceCore = "io.circe" %% "circe-core" % circeVersion
    val CirceGeneric = "io.circe" %% "circe-generic" % circeVersion
    val CirceParser = "io.circe" %% "circe-parser" % circeVersion
    val CirceYaml = "io.circe" %% "circe-yaml" % "0.14.2"

    val All = Seq(CirceCore, CirceGeneric, CirceParser, CirceYaml)
  }

  object Logging {
    val slf4jversion   = "1.7.32"
    val logbackVersion = "1.2.9"

    val Slf4jApi = "org.slf4j"       % "slf4j-api"       % slf4jversion
    val Logback  = ("ch.qos.logback" % "logback-classic" % logbackVersion).exclude("org.slf4j", "slf4j")
  }
}
