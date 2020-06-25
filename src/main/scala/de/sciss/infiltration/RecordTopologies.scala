/*
 *  RecordTopologies.scala
 *  (Infiltration)
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

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth.InMemory
import de.sciss.nuages.{Nuages, ScissProcs, Wolkenpumpe, WolkenpumpeMain}
import de.sciss.submin.Submin
import de.sciss.synth.Server
import de.sciss.synth.proc.Proc
import de.sciss.synth.proc.Implicits._

import scala.swing.{Button, Swing}

object RecordTopologies {
  def main(args: Array[String]): Unit = {
    Wolkenpumpe.init()
    Swing.onEDT {
      Submin.install(true)
      run()
    }
  }

  def dumpTopology[S <: Sys[S]](n: Nuages[S])(implicit tx: S#Tx): Unit = {
    n.surface match {
      case Nuages.Surface.Folder(f) =>
        f.iterator.foreach {
          case p: Proc[S] =>
            println(p.name)

          case other =>
            println(s"(ignoring $other)")
        }

      case Nuages.Surface.Timeline(_) =>
        sys.error("Timeline not supported")
    }
  }

  def run(): Unit = {
    type S = InMemory
    implicit val system: S = InMemory()
    val w: WolkenpumpeMain[S] = new WolkenpumpeMain[S] {
      override protected def configure(sCfg: ScissProcs.ConfigBuilder, nCfg: Nuages.ConfigBuilder,
                                       aCfg: Server.ConfigBuilder): Unit = {
        super.configure(sCfg, nCfg, aCfg)
        sCfg.genNumChannels = 4
        nCfg.masterChannels = Some(0 until 4)
        nCfg.soloChannels   = None
        nCfg.lineInputs     = Vector.empty
        nCfg.lineOutputs    = Vector.empty
      }
    }

    val nuagesH = system.step { implicit tx => tx.newHandle(Nuages.folder[S]) }
    w.run(nuagesH)

    w.view.addSouthComponent(Button("Dump Topology") {
      system.step { implicit tx =>
        dumpTopology(nuagesH())
      }
    })
  }
}
