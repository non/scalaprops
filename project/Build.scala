import sbt._, Keys._
import sbtunidoc.Plugin._
import Common._

object build extends Build {

  private[this] val coreName = "scalaprops-core"
  private[this] val allName = "scalaprops-all"
  private[this] val scalazlawsName = "scalaprops-scalazlaws"
  private[this] val spirelawsName = "scalaprops-spirelaws"
  private[this] val scalapropsName = "scalaprops"

  private[this] val scalazVersion = "7.1.2"

  val modules: List[String] = (
    coreName ::
    allName ::
    scalazlawsName ::
    spirelawsName ::
    scalapropsName ::
    Nil
  )

  private[this] def module(id: String) =
    Project(id, file(id)).settings(commonSettings).settings(
      initialCommands in console += "import scalaprops._, scalaz._"
    )

  lazy val core = module("core").settings(
    Generator.settings
  ).settings(
    name := coreName,
    libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion
  )

  lazy val scalazlaws = module("scalazlaws").settings(
    name := scalazlawsName
  ).dependsOn(core)

  lazy val spirelaws = module("spirelaws").settings(
    name := spirelawsName,
    libraryDependencies += "org.spire-math" %% "spire" % "0.10.1",
    testFrameworks += new TestFramework("scalaprops.ScalapropsFramework"),
    parallelExecution in Test := false
  ).dependsOn(core)

  lazy val scalaprops = module(scalapropsName).settings(
    name := scalapropsName,
    libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0",
    libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % scalazVersion,
    shapelessDependency("test"),
    testFrameworks += new TestFramework("scalaprops.ScalapropsFramework"),
    parallelExecution in Test := false
  ).dependsOn(core, scalazlaws % "test")

  val sxr = TaskKey[File]("packageSxr")

  import UnidocKeys._

  val root = Project("root", file(".")).settings(
    commonSettings ++
    unidocSettings ++
    xerial.sbt.Sonatype.sonatypeRootSettings ++ (
      core ::
      scalaprops ::
      scalazlaws ::
      Nil
    ).map(libraryDependencies <++= libraryDependencies in _)
  ).settings(
    name := allName,
    artifacts := Nil,
    packagedArtifacts := Map.empty,
    artifacts <++= Classpaths.artifactDefs(Seq(packageDoc in Compile)),
    packagedArtifacts <++= Classpaths.packaged(Seq(packageDoc in Compile)),
    scalacOptions in UnidocKeys.unidoc += {
      "-P:sxr:base-directory:" + (sources in UnidocKeys.unidoc in ScalaUnidoc).value.mkString(":")
    }
  ).settings(
    Defaults.packageTaskSettings(
      packageDoc in Compile, (UnidocKeys.unidoc in Compile).map{_.flatMap(Path.allSubpaths)}
    ) ++ Defaults.packageTaskSettings(
      sxr in Compile, (crossTarget in Compile).map{ dir =>
        Path.allSubpaths(dir / "unidoc.sxr").toSeq
      }
    )
  ).settings(
    resolvers += "bintray/paulp" at "https://dl.bintray.com/paulp/maven",
    addCompilerPlugin("org.improving" %% "sxr" % "1.0.1"),
    sxr in Compile <<= (sxr in Compile).dependsOn(compile in Compile),
    packagedArtifacts <++= Classpaths.packaged(Seq(sxr in Compile)),
    artifacts <++= Classpaths.artifactDefs(Seq(sxr in Compile)),
    artifactClassifier in sxr := Some("sxr"),
    sxr in Compile <<= (sxr in Compile).dependsOn(UnidocKeys.unidoc in Compile)
  ).aggregate(
    core, scalaprops, scalazlaws
  )

}
