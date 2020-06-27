lazy val commonSettings = Seq(
  name               := "Infiltration",
  version            := "0.1.0-SNAPSHOT",
  organization       := "de.sciss",
  scalaVersion       := "2.13.1",
  licenses           := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  homepage           := Some(url(s"https://git.iem.at/sciss/${name.value}")),
  scalacOptions     ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xsource:2.13", "-Xlint:-stars-align,_"),
  scalacOptions      += "-Yrangepos",  // this is needed to extract source code
  updateOptions      := updateOptions.value.withLatestSnapshots(false),
  test in assembly := {},
)

lazy val deps = new {
  val fscape      = "2.36.1"
  val lucre       = "3.17.1"
  val melliteApp  = "2.48.0"
  val negatum     = "0.15.1"
  val submin      = "0.3.4"
//  val wolkenpumpe = "2.40.0"
}

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "de.sciss" %% "fscape-macros" % deps.fscape,
      "de.sciss" %% "fscape-views"  % deps.fscape,
      "de.sciss" %% "lucre-bdb"     % deps.lucre,
      "de.sciss" %% "lucre-expr"    % deps.lucre,
      "de.sciss" %% "mellite-app"   % deps.melliteApp,
      "de.sciss" %% "negatum-core"  % deps.negatum,
      "de.sciss" %  "submin"        % deps.submin,
//      "de.sciss" %% "wolkenpumpe"   % deps.wolkenpumpe,
    ),
    mainClass             in assembly := Some("de.sciss.infiltration.RunNegatum"),
    assemblyJarName       in assembly := "RunNegatum.jar",
    target                in assembly := baseDirectory.value,
    assemblyMergeStrategy in assembly := {
      case PathList("org", "xmlpull", _ @ _*) => MergeStrategy.first
      case PathList("org", "w3c", "dom", "events", _ @ _*) => MergeStrategy.first // bloody Apache Batik
      case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.first
      case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
    }
  )
