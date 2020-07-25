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
import java.awt.geom.Point2D
import java.net.InetSocketAddress

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Copy, Folder, Obj}
import de.sciss.lucre.synth.{Server, Sys}
import de.sciss.nuages.NuagesView
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Grapheme, Output, Proc}

class InfMain[S <: Sys[S], I <: Sys[I]](
                            view: NuagesView[I],
                            server            : Server,
                            localSocketAddress: InetSocketAddress,
                            config            : Config,
                            grProcH           : stm.Source[S#Tx, Grapheme[S]],
                            numProcs          : Int,
                          )(implicit cursor: stm.Cursor[S], /*cursorI: stm.Cursor[I],*/ bridge: S#Tx => I#Tx)
  extends /*Main with*/ SoundScene {

  private[this] val panel = view.panel

  def init()(implicit tx: S#Tx): this.type = {
    tx.afterCommit {
      val c = OscClient(this, config, localSocketAddress)
      c.init()
    }
    runProgram()
//    deferBi { (tx, itx) => runProgram() }
    this
  }

  def changeNegatum()(implicit tx: S#Tx): Unit = {
    implicit val itx: I#Tx = bridge(tx)
    val nOpt = panel.nodes.find(_.name.startsWith("n-"))
    nOpt.fold[Unit](println("Ooops. Not found")) { nGenOld =>
      val pIdx = (math.random() * numProcs).toInt
      val grProc  = grProcH()
      val pGen0SOpt  = grProc.at(pIdx).flatMap { e =>
        e.value match {
          case p: Proc[S] => Some(p)
          case _ => None
        }
      }

      pGen0SOpt.foreach { pGen0S =>
//        val pOld = nGenOld.obj

        val mapOut: Map[String, Set[(Obj.AttrMap[I], String)]] = nGenOld.outputs.map { case (keyOut, nOutOld) =>
          val outOld  = nOutOld.output
          val set = nOutOld.mappings.flatMap { nAttrIn =>
            val nAttr = nAttrIn.attribute
            val keyIn = nAttr.key
            val pCol  = nAttr.parent.obj //  nAttrIn.input
            val aCol  = pCol.attr
            aCol.get(keyIn) match {
              case Some(f: Folder[I]) =>
//                if (f.size > 1) f.remove(outOld) else aCol.remove(keyIn)
                Some((aCol, keyIn))

              case Some(o: Output[I]) if o == outOld =>
//                aCol.remove(keyIn)
                Some((aCol, keyIn))

              case _ =>
                println("Huh?")
                None
            }
          }

          keyOut -> set
        }

        val cpy     = Copy[S, I]
        val pGenNew = cpy(pGen0S)
        cpy.finish()

        if (config.display) {
          val aggrOld = nGenOld.aggregate
          if (aggrOld != null) {
            val bOld  = aggrOld.getBounds
            val ptNew = new Point2D.Double(bOld.getCenterX, bOld.getCenterY)
            panel.prepareAndLocate(pGenNew, ptNew)
          }
        }

        nGenOld.removeSelf()

//        panel.nuages.surface match {
//          case fRoot: Nuages.Surface.Folder[I] =>
//            fRoot.peer.remove(pOld)
//
//          case _: Nuages.Surface.Timeline[I] => ???
//        }
        panel.addNewObject(pGenNew)

        mapOut.foreach { case (keyOut, set) =>
          val outNew = pGenNew.outputs.add(keyOut)
          set.foreach { case (aCol, keyIn) =>
            aCol.get(keyIn) match {
              case Some(f: Folder[I]) => f.addLast(outNew)
              case Some(other) =>
                val f = Folder[I]()
                f.addLast(other)
                f.addLast(outNew)
                aCol.put(keyIn, f)
              case None =>
                aCol.put(keyIn, outNew)
            }
          }
        }
      }
    }
  }

  private def runProgram()(implicit tx: S#Tx): Unit = {
    val pIdx = (math.random() * numProcs).toInt
    implicit val itx: I#Tx = bridge(tx)
    val n         = panel.nuages
    val fGenOpt   = n.generators
    val fColOpt   = n.collectors
    val grProc    = grProcH()

    val programOpt = for {
      //      pGen0 <- fGenOpt.flatMap(_.$[Proc]("negatum-8bbebbf4"))
      pIn0    <- fGenOpt.flatMap(_.$[Proc]("in"))
      pGen0S  <- grProc.at(pIdx).flatMap { e =>
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
      panel.createGenerator(pGen0 , Some(pCol0) , new Point(200, 200))
      panel.createGenerator(pIn0  , None        , new Point(400, 200))

      ()
    }

    if (programOpt.isEmpty) {
      println("! No generator folder")
    }
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
