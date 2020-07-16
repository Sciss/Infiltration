/*
 *  OptimizeWorkspace.scala
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

import java.util.{Timer, TimerTask}

import de.sciss.file._
import de.sciss.kollflitz.Vec
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Folder
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.{Application, Mellite}
import de.sciss.negatum.Optimize
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Durable, Proc, Workspace}

import scala.util.{Failure, Success}

object OptimizeWorkspace {
  case class ProcSpec(name: String, graph: SynthGraph)

  def main(args: Array[String]): Unit = {
    require (args.length == 1, "Must provide a workspace .mllt argument")
    val wsDir = file(args(0))

    Application.init(Mellite)
    Mellite.initTypes()
    RunNegatum.tweak()

    type S  = Durable
    val dsf = BerkeleyDB.factory(wsDir, createIfNecessary = false)
    val _ws = Workspace.Durable.read(wsDir, dsf)
    val (iterMap: Map[Int, Vec[ProcSpec]], folderOutH) = _ws.system.step { implicit tx =>
      val r         = _ws.root
      val folderIn  = r.$[Folder]("out").getOrElse(sys.error("No folder 'out' found"))
      val folderOut = r.$[Folder]("opt").getOrElse {
        val f = Folder[S]()
        f.name = "opt"
        r.addLast(f)
        f
      }
      val _iterMap = folderIn.iterator.collect {
        case folderIt: Folder[S] =>
          val itName  = folderIt.name
          val iterIdx = {
            val i = itName.indexOf(' ')
            itName.substring(i + 1).toInt
          }

          val procs = folderIt.iterator.collect {
            case p: Proc[S] =>
              ProcSpec(p.name, p.graph.value)
          } .toIndexedSeq

          (iterIdx, procs)
      } .toMap

      (_iterMap, tx.newHandle(folderOut))
    }

    run(_ws, iterMap, folderOutH = folderOutH)
  }

  def run[S <: Sys[S]](ws: Workspace[S], iterMap: Map[Int, Vec[ProcSpec]],
                       folderOutH: stm.Source[S#Tx, Folder[S]]): Unit = {
    val iterKeys0 = iterMap.keys.toIndexedSeq.sorted
    import de.sciss.mellite.Mellite.executionContext
    import ws.cursor

    val timer = new Timer
    val numExisting = cursor.step { implicit tx => folderOutH().size }
    println(s"Iterations: ${numExisting} of ${iterKeys0.size}")

    val iterKeys = iterKeys0.drop(numExisting)
    val futAll = Parametrize.sequence(iterKeys) { iterIdx =>
      println(s"Iteration $iterIdx")
      val folderItH = cursor.step { implicit tx =>
        val folderOut = folderOutH()
        val folderIt  = Folder[S]()
        folderIt.name = s"It $iterIdx"
        folderOut.addLast(folderIt)
        tx.newHandle(folderIt)
      }
      val procs = iterMap(iterIdx)
      Parametrize.sequence(procs) { procSpec =>
        println(s"   ${procSpec.name}")
        val oCfg = Optimize.Config(
          graph         = procSpec.graph,
          sampleRate    = 44100,
          analysisDur   = 5.9,
          blockSize     = 64,
          expandProtect = false, // true,
          expandIO      = false, // true,
        )
        val o = Optimize(oCfg)
        o.start()

        val ttTimeOut = new TimerTask {
          def run(): Unit = o.abort()
        }
        timer.schedule(ttTimeOut, 10000L)

//        o.onComplete(_ => ttTimeOut.cancel())

        o.transform { tr =>
          ttTimeOut.cancel()
          tr match {
            case Success(res) =>
              println(s"Optimization found ${res.numConst} constant replacement and ${res.numEqual} redundant elements.")
              cursor.step { implicit tx =>
                val folderIt  = folderItH()
                val pOpt      = Proc[S]()
                pOpt.name     = s"${procSpec.name}-opt"
                pOpt.graph()  = res.graph
                folderIt.addLast(pOpt)
              }

            case Failure(ex) =>
              println(s"... failed ${ex.getClass.getSimpleName}")
          }

//          procSpec.copy(graph = res.graph)
          Success(()) // ignore errors
        }
      }
    }

    futAll.onComplete { tr =>
      println(tr)
      ws.close()
      sys.exit(if (tr.isSuccess) 0 else 1)
    }
  }
}
