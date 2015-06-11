import sbt._, Keys._
import sbtunidoc.Plugin._
import Common._

object build extends Build {

  private[this] val coreName = "scalaprops-core"
  private[this] val allName = "scalaprops-all"
  private[this] val scalazlawsName = "scalaprops-scalazlaws"
  private[this] val scalapropsName = "scalaprops"

  private[this] val scalazVersion = "7.1.2"

  val modules: List[String] = (
    coreName ::
    allName ::
    scalazlawsName ::
    scalapropsName ::
    Nil
  )

  private[this] def module(id: String) =
    Project(id, file(id)).settings(commonSettings).settings(
      initialCommands in console += "import scalaprops._, scalaz._"
    )

  private[this] val quasiquotesVersion = "2.1.0-M5"

  lazy val core = module("core").settings(
    Generator.settings
  ).settings(
    name := coreName,
    libraryDependencies ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)){
      case Some((2, 10)) => List(
        "org.scalamacros" %% "quasiquotes" % quasiquotesVersion % "test",
        compilerPlugin("org.scalamacros" % "paradise" % quasiquotesVersion cross CrossVersion.full)
      )
    }.toList.flatten,
    unmanagedSourceDirectories in Test += {
      val dir = CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 10)) => "scala210"
        case _ => "scala211"
      }
      (sourceDirectory in Test).value / dir
    },
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "test",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion
  )

  lazy val scalazlaws = module("scalazlaws").settings(
    name := scalazlawsName
  ).dependsOn(core % "compile->compile;test->test")

  lazy val scalaprops = module(scalapropsName).settings(
    name := scalapropsName,
    libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0",
    libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % scalazVersion,
    shapelessDependency("test"),
    testFrameworks += new TestFramework("scalaprops.ScalapropsFramework"),
    parallelExecution in Test := false
  ).dependsOn(core % "compile->compile;test->test", scalazlaws % "test")

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
