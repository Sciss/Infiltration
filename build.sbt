lazy val commonSettings = Seq(
  name               := "Infiltration",
  version            := "0.1.0-SNAPSHOT",
  organization       := "de.sciss",
  scalaVersion       := "2.13.1",
  licenses           := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  homepage           := Some(url(s"https://git.iem.at/sciss/${name.value}")),
  scalacOptions     ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xsource:2.13", "-Xlint:-stars-align,_"),
  scalacOptions      += "-Yrangepos",  // this is needed to extract source code
  updateOptions      := updateOptions.value.withLatestSnapshots(false)
)

lazy val deps = new {
  val wolkenpumpe = "2.40.0"
  val submin      = "0.3.4"
}

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "de.sciss" %% "wolkenpumpe" % deps.wolkenpumpe,
      "de.sciss" %  "submin"      % deps.submin,
    ),
  )
