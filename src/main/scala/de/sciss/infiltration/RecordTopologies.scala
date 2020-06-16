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

import de.sciss.lucre.synth.InMemory
import de.sciss.nuages.{Nuages, ScissProcs, Wolkenpumpe, WolkenpumpeMain}
import de.sciss.submin.Submin
import de.sciss.synth.Server

import scala.swing.Swing

object RecordTopologies {
  def main(args: Array[String]): Unit = {
    Wolkenpumpe.init()
    Swing.onEDT {
      Submin.install(true)
      run()
    }
  }

  def run(): Unit = {
    type S = InMemory
    implicit val system: S = InMemory()
    val w = new WolkenpumpeMain[S] {
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

    val nuagesH = system.step { implicit tx => tx.newHandle(Nuages.timeline[S]) }
    w.run(nuagesH)
  }
}
