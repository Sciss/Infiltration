/*
 *  InfMain.scala
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

import java.awt.Point
import java.net.InetSocketAddress

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Copy, Txn}
import de.sciss.lucre.synth.{Server, Sys}
import de.sciss.nuages.NuagesView
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Grapheme, Proc, SoundProcesses}

import scala.concurrent.Future

class InfMain[S <: Sys[S], I <: Sys[I]](
                            view: NuagesView[I],
                            server            : Server,
                            localSocketAddress: InetSocketAddress,
                            config            : Config,
                            grProcH           : stm.Source[S#Tx, Grapheme[S]]
                          )(implicit cursor: stm.Cursor[S], cursorI: stm.Cursor[I], bridge: S#Tx => I#Tx)
  extends Main with SoundScene {

  private[this] val panel = view.panel

  def init()(implicit tx: S#Tx): this.type = {
    tx.afterCommit {
      val c = OscClient(this, this, config, localSocketAddress)
      c.init()
      runProgram()
    }
    this
  }

  private def runProgram(): Unit = {
    Future {
      Txn.copy[S, I, Unit] { (_tx, _itx) =>
        implicit val tx : S#Tx = _tx
        implicit val itx: I#Tx = _itx // bridge(tx)
        val n         = panel.nuages
        //    val fGenOpt   = n.generators
        val fColOpt   = n.collectors
        val grProc    = grProcH()

        val programOpt = for {
          //      pGen0 <- fGenOpt.flatMap(_.$[Proc]("negatum-8bbebbf4"))
          pGen0S <- grProc.at(333).flatMap { e =>
            e.value match {
              case p: Proc[S] => Some(p)
              case _ => None
            }
          }
          pCol0 <- fColOpt.flatMap(_.$[Proc]("O-inf" /*"O-all"*/))
        } yield {
//          val g0 = pGen0S.graph.value
//          val pGen0 = Proc[I]()
//          pGen0.graph() = g0
          val cpy   = Copy[S, I]
          val pGen0 = cpy(pGen0S)
          cpy.finish()
          //      val pCol = panel.createGenerator(pCol0, None, new Point(100, 100))
          panel.createGenerator(pGen0, Some(pCol0), new Point(200, 200))
          ()
        }

        if (programOpt.isEmpty) {
          println("! No generator folder")
        }

        ()
      }
    } (SoundProcesses.executionContext)
  }

  var showLog: Boolean = config.log

  def fullVersion: String = "v0.1.0-SNAPSHOT"

  private def stepI[A](f: I#Tx => A): A =
    cursor.step { implicit tx =>
      val itx: I#Tx = bridge(tx)
      f(itx)
    }

  def setMainVolume(amp: Double): Unit =
    stepI { implicit tx =>
      panel.setMainVolume(amp)
    }

  def start(): Unit =
    stepI { implicit tx =>
      panel.transport.play()
    }

  def stop(): Unit =
    stepI { implicit tx =>
      panel.transport.stop()
    }

  def testChannel(ch: Int): Boolean = {
    println("testChannel")
    false
  }

  def serverInfo(): String =
    server.peer.counts.toString
}
