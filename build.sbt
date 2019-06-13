import org.scalajs.core.tools.linker.ModuleKind
import sbt.Keys.{libraryDependencies, resolvers}
import sbtcrossproject.CrossPlugin.autoImport.crossProject

val ivyLocal = Resolver.file("ivy", file(Path.userHome.absolutePath + "/.ivy2/local"))(Resolver.ivyStylePatterns)

name := "amf-core"

version in ThisBuild := {
  val major = 4
  val minor = 0

  lazy val build = sys.env.getOrElse("BUILD_NUMBER", "0")
  lazy val branch = sys.env.get("BRANCH_NAME")

  if (branch.contains("master")) s"$major.$minor.$build" else s"$major.${minor + 1}.0-SNAPSHOT"
}

publish := {}

jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()

lazy val sonarUrl = sys.env.getOrElse("SONAR_SERVER_URL", "Not found url.")
lazy val sonarToken = sys.env.getOrElse("SONAR_SERVER_TOKEN", "Not found token.")

enablePlugins(SonarRunnerPlugin)

sonarProperties ++= Map(
  "sonar.host.url" -> sonarUrl,
  "sonar.login" -> sonarToken,
  "sonar.projectKey" -> "mulesoft.amf-core",
  "sonar.projectName" -> "AMF-CORE",
  "sonar.projectVersion" -> "4.0.0",
  "sonar.sourceEncoding" -> "UTF-8",
  "sonar.github.repository" -> "mulesoft/amf-core",
  "sonar.sources" -> "shared/src/main/scala",
  "sonar.tests" -> "shared/src/test/scala",
  "sonar.scala.coverage.reportPaths" -> "jvm/target/scala-2.12/scoverage-report/scoverage.xml"
)

val settings = Common.settings ++ Common.publish ++ Seq(
  organization := "com.github.amlorg",
  resolvers ++= List(ivyLocal,
    Common.releases,
    Common.snapshots,
    Resolver.mavenLocal),
  resolvers += "jitpack" at "https://jitpack.io",
  credentials ++= Common.credentials(),
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % "3.0.5" % Test,
    "com.github.scopt" %%% "scopt" % "3.7.0"
  )
)

/** **********************************************
  * AMF-Core
  * ********************************************* */
lazy val workspaceDirectory: File =
  sys.props.get("sbt.mulesoft") match {
    case Some(x) => file(x)
    case _ => Path.userHome / "mulesoft"
  }

val syamlVersion = "0.7.240"

lazy val syamlJVMRef = ProjectRef(workspaceDirectory / "syaml", "syamlJVM")
lazy val syamlJSRef = ProjectRef(workspaceDirectory / "syaml", "syamlJS")
lazy val syamlLibJVM = "org.mule.syaml" %% "syaml" % syamlVersion
lazy val syamlLibJS = "org.mule.syaml" %% "syaml_sjs0.6" % syamlVersion

lazy val defaultProfilesGenerationTask = TaskKey[Unit](
  "defaultValidationProfilesGeneration",
  "Generates the validation dialect documents for the standard profiles")

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .settings(
    Seq(
      name := "amf-core"
    ))
  .in(file("."))
  .settings(settings)
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided",
    libraryDependencies += "org.scala-lang.modules" % "scala-java8-compat_2.12" % "0.8.0",
    libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.4",
    artifactPath in(Compile, packageDoc) := baseDirectory.value / "target" / "artifact" / "amf-core-javadoc.jar"
  )
  .jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2",
    scalaJSModuleKind := ModuleKind.CommonJSModule,
    artifactPath in(Compile, fullOptJS) := baseDirectory.value / "target" / "artifact" / "amf-core-module.js"
  )

lazy val coreJVM = core.jvm.in(file("./jvm")).sourceDependency(syamlJVMRef, syamlLibJVM)
lazy val coreJS = core.js.in(file("./js")).sourceDependency(syamlJSRef, syamlLibJS)
