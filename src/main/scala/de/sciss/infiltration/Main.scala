/*
 *  Main.scala
 *  (in|filtration)
 *
 *  Copyright (c) 2019-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.infiltration

object Main {
  val map: Map[String, Array[String] => Unit] = Map(
    "infiltration"    -> Infiltration     .main,
    "copy-folder"     -> CopyFolder       .main,
    "hilbert-curve"   -> HilbertCurveTest .main,
    "optimize-test"   -> OptimizeTest     .main,
    "optimize"        -> OptimizeWorkspace.main,
    "parametrize"     -> Parametrize      .main,
    "topologies"      -> RecordTopologies .main,
    "negatum"         -> RunNegatum       .main,
    "trunk-to-sound"  -> TrunkToSoundSeq  .main,
    "play-chain"      -> PlayChain        .main,
    "remove-simple"   -> RemoveSimpleProcs.main,
    "make-chains"     -> MakeChains       .main,
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
trait Main {
  var showLog: Boolean

  def fullVersion: String
}