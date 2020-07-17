package de.sciss.infiltration

object Main {
  val map: Map[String, Array[String] => Unit] = Map(
    "copy-folder"     -> CopyFolder       .main,
    "hilbert-curve"   -> HilbertCurveTest .main,
    "optimize-test"   -> OptimizeTest     .main,
    "optimize"        -> OptimizeWorkspace.main,
    "parametrize"     -> Parametrize      .main,
    "topologies"      -> RecordTopologies .main,
    "negatum"         -> RunNegatum       .main,
    "trunk-to-sound"  -> TrunkToSoundSeq  .main,
  )

  def main(args: Array[String]): Unit = {
    val sub = args.headOption.getOrElse("")
    map.get(sub) match {
      case Some(subProgram) => subProgram(args.drop(1))
      case None =>
        println("Must specify a valid sub-program:")
        println(map.keys.toList.sorted.mkString(", "))
        sys.exit(1)
    }
  }
}
