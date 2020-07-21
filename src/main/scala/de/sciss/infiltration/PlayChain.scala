/*
 *  PlayChain.scala
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

import de.sciss.file._
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.{Server, Txn}
import de.sciss.mellite.Mellite
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{AuralSystem, Durable, Grapheme, Runner, SoundProcesses, TimeRef, Universe, Workspace}
import de.sciss.lucre.stm.TxnLike.peer

import scala.concurrent.stm.Ref

object PlayChain {
  def main(args: Array[String]): Unit = {
    require (args.length == 1, "must specify workspace")

    Parametrize.init()

    val wsInDir = file(args(0))
    type S    = Durable
    val dbIn  = BerkeleyDB.factory(wsInDir, createIfNecessary = false)
    implicit val wsIn: Workspace[S] = Workspace.Durable.read(wsInDir, dbIn)
    implicit val cursor: stm.Cursor[S] = wsIn.cursor
    val circleName = "circle"
    val (circleH, numProcs, _universe) = cursor.step { implicit tx =>
      val r         = wsIn.root
      val grapheme  = r.$[Grapheme](circleName).getOrElse(sys.error(s"No grapheme '$circleName' found"))
      val _num      = grapheme.lastEvent.getOrElse(sys.error("Huh, empty?")).toInt
      val _u        = Universe[S]()
      (tx.newHandle(grapheme), _num, _u)
    }

    println(s"numProcs = $numProcs")

    implicit val u: Universe[S] = _universe
    val sch = u.scheduler

    val procIdxR  = Ref(150)
    val runnerR   = Ref(List.empty[Runner[S]])

    def play()(implicit tx: S#Tx): Unit = {
      val g     = circleH()
      val procIdx = procIdxR.getAndTransform(_ + 1)
      if (procIdx < numProcs) {
        val proc  = g.at(procIdx).getOrElse(sys.error("Woopa")).value
        println(s"play($procIdx) = ${proc.name}")
        val r     = Runner(proc)
        r.run()
        val rL = r :: runnerR()
        val (rKeep, rFree) = rL.splitAt(4)
        runnerR() = rKeep
        rFree.foreach(_.dispose())
        sch.schedule(sch.time + (TimeRef.SampleRate * 3).toLong) { implicit tx =>
          play()
        }
      } else {
        tx.afterCommit {
          println("Und tschüß")
          wsIn.close()
          sys.exit()
        }
      }
    }

    cursor.step { implicit tx =>
      val as = Mellite.auralSystem
      as.start()
      as.addClientNow(new AuralSystem.Client {
        def auralStarted(s: Server)(implicit tx: Txn): Unit = {
          tx.afterCommit {
            s.peer.dumpOSC()
            SoundProcesses.step[S]("start") { implicit tx =>
              play()
            }
          }
        }

        def auralStopped()(implicit tx: Txn): Unit = ()
      })
    }

//    wsIn.close()
//    sys.exit()
  }
}
