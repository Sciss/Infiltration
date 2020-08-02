lazy val commonSettings = Seq(
  name               := "Infiltration",
  version            := "0.1.5",
  organization       := "de.sciss",
  scalaVersion       := "2.13.3",
  licenses           := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  homepage           := Some(url(s"https://git.iem.at/sciss/${name.value}")),
  scalacOptions     ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xsource:2.13", "-Xlint:-stars-align,_"),
  scalacOptions      += "-Yrangepos",  // this is needed to extract source code
  updateOptions      := updateOptions.value.withLatestSnapshots(false),
  test in assembly := {},
)

lazy val buildInfoSettings = Seq(
  // ---- build info ----
  buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
    BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
    BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
  ),
  buildInfoOptions += BuildInfoOption.BuildTime
)

lazy val deps = new {
  val fscape          = "2.36.1"
  val lucre           = "3.17.6"
  val melliteApp      = "2.48.2"
  val negatum         = "0.15.5"
  val soundProcesses  = "3.35.8"
  val submin          = "0.3.4"
  val wolkenpumpe     = "2.41.3"
  val pi4j            = "1.2"
}

lazy val root = project.in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(buildInfoSettings)
  .settings(
    libraryDependencies ++= Seq(
      "de.sciss" %% "fscape-macros" % deps.fscape,
      "de.sciss" %% "fscape-views"  % deps.fscape,
      "de.sciss" %% "lucre-core"    % deps.lucre,
      "de.sciss" %% "lucre-bdb"     % deps.lucre,
      "de.sciss" %% "lucre-expr"    % deps.lucre,
      "de.sciss" %% "mellite-app"   % deps.melliteApp,
      "de.sciss" %% "negatum-core"  % deps.negatum,
      "de.sciss" %% "soundprocesses-core"  % deps.soundProcesses,
      "de.sciss" %  "submin"        % deps.submin,
      "de.sciss" %% "wolkenpumpe"   % deps.wolkenpumpe,
      "com.pi4j" %  "pi4j-core"     % deps.pi4j
    ),
    mainClass             in assembly := Some("de.sciss.infiltration.Main"),
    assemblyJarName       in assembly := "Infiltration.jar",
    target                in assembly := baseDirectory.value,
    assemblyMergeStrategy in assembly := {
      case PathList("org", "xmlpull", _ @ _*) => MergeStrategy.first
      case PathList("org", "w3c", "dom", "events", _ @ _*) => MergeStrategy.first // bloody Apache Batik
      case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.first
      case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
    },
    buildInfoPackage := "de.sciss.infiltration",
  )
