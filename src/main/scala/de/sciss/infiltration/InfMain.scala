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
import de.sciss.lucre.synth.{Server, Sys}
import de.sciss.nuages.NuagesView
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.Proc

class InfMain[S <: Sys[S]](
                            view: NuagesView[S],
//                            transport         : Transport[S],
                            server            : Server,
                            localSocketAddress: InetSocketAddress,
                            config            : Config,
                          )(implicit cursor: stm.Cursor[S])
  extends Main with SoundScene {

  private[this] val panel = view.panel

  def init()(implicit tx: S#Tx): this.type = {
    val n         = panel.nuages
    val fGenOpt   = n.generators
    val fColOpt   = n.collectors

    val programOpt = for {
      pGen0 <- fGenOpt.flatMap(_.$[Proc]("negatum-8bbebbf4"))
      pCol0 <- fColOpt.flatMap(_.$[Proc]("O-inf" /*"O-all"*/))
    } yield {
//      val pCol = panel.createGenerator(pCol0, None, new Point(100, 100))
      panel.createGenerator(pGen0, Some(pCol0), new Point(100, 100))
      ()
    }

    if (programOpt.isEmpty) {
      println("! No generator folder")
    }

    tx.afterCommit {
      val c = OscClient(this, this, config, localSocketAddress)
      c.init()
    }
    this
  }

  var showLog: Boolean = config.log

  def fullVersion: String = "v0.1.0-SNAPSHOT"

  def setMainVolume(amp: Double): Unit =
    cursor.step { implicit tx =>
      panel.setMainVolume(amp)
//      mainSynth.set("amp" -> amp)
    }

  def start(): Unit =
    cursor.step { implicit tx =>
//      val t = view.panel.transport
//    t.stop()
//    t.seek(0L)
      panel.transport.play()
    }

  def stop(): Unit =
    cursor.step { implicit tx =>
//      val t = view.panel.transport
      panel.transport.stop()
    }

  def testChannel(ch: Int): Boolean = {
    println("testChannel")
    false
  }

  def serverInfo(): String =
    server.peer.counts.toString
}
