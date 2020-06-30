/*
 *  SelectionTest.scala
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
import de.sciss.fscape.Graph
import de.sciss.kollflitz.Vec
import de.sciss.lucre.stm.Folder
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.mellite.{Application, Mellite}
import de.sciss.negatum.impl.Evaluation
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Durable, Proc, Workspace}

import scala.concurrent.{ExecutionContext, Future}

object SelectionTest {
  val projectDir  : File  = file("/") / "data" / "projects" / "Infiltration"
  val workspaceDir: File  = projectDir / "workspaces"
  val audioDir    : File  = projectDir / "audio_work"

  val trunkIdMap = Map(
    11 -> "47e8301c",
    12 -> "8447ab15",
    13 -> "4b700021",
    14 -> "fd55ed28",
    15 -> "49373ce1",
    18 -> "55d91895",
  )

  def main(args: Array[String]): Unit = {
    Application.init(Mellite)
    Mellite.initTypes()

    //    val trunkId = 11
    //    bounceAll()

    testCorr()
  }

  def any2stringadd(in: Any): Any = ()

  // N.B. we have implemented this directly as a workspace now
  def testCorr(): Unit = {
    val bncFiles = (audioDir / "def_bnc").children(_.extL == "aif").sorted
    println(s"Found ${bncFiles.size} files.")

    bncFiles.take(2).combinations(2).foreach { case Seq(f1, f2) =>
      val g: Graph = Graph {
        import de.sciss.fscape.graph.{AudioFileIn => _, _}
        import de.sciss.fscape.lucre.graph._
        //val numFrames = 262144
        val in1       = AudioFileIn("in-1")//.take(numFrames)
        val in2       = AudioFileIn("in-2")//.take(numFrames)
        val in2Rvs    = ReverseWindow(in2, in2.numFrames)
        val convSize  = in1.numFrames + in2.numFrames - 2
        val fftSize   = convSize.nextPowerOfTwo // numFrames << 1
        //fftSize.poll("fftSize")
        val fft1      = Real1FFT(in1    , size = fftSize, mode = 1) * fftSize
        val fft2      = Real1FFT(in2Rvs , size = fftSize, mode = 1)
        val e1        = RunningSum(in1.squared).last
        val e2        = RunningSum(in2.squared).last
        val prod      = fft1.complex * fft2
        val corr      = Real1IFFT(prod, fftSize, mode = 1)
        //Plot1D(corr, convSize min 1024, "corr")
        val corrMax0  = RunningMax(corr.abs).last
        val corrMax = corrMax0 / (e1 + e2)
        corrMax.ampDb.poll("corrMax [dB]")
        //e1.poll("e1")
        //e2.poll("e2")
      }


    }
  }

  def bounceAll(trunkId: Int): Unit = {
    type S = Durable
    val wsDir       = workspaceDir / "pi5" / s"NegatumTrunk$trunkId.mllt"
    val dbf         = BerkeleyDB.factory(wsDir, createIfNecessary = false)
    val ws = Workspace.Durable.read(wsDir, dbf)
    val pairs: Vec[(String, SynthGraph)] = ws.cursor.step { implicit tx =>
      ws.root.iterator.collectFirst {
        case f: Folder[S] if f.name == "It 32" =>
          f.iterator.collect {
            case p: Proc[S] => (p.name, p.graph().value)
          } .toIndexedSeq
      } .get
    }
    ws.close()

    println(s"Got ${pairs.size} graphs.")

    bounceAll(pairs, trunkId = trunkId)
  }

  def sequence[A, B](xs: Seq[A])(f: A => Future[B])(implicit exec: ExecutionContext): Future[Seq[B]] =
    xs.foldLeft(Future.successful(Vector.empty[B])) {
      case (acc, x) =>
        acc.flatMap(prev => f(x).map(prev :+ _))
    }

  def bounceAll(pairs: Vec[(String, SynthGraph)], trunkId: Int): Unit = {
//    val numFrames = 262144

    import Mellite.executionContext

    val tempSpec = AudioFile.readSpec(audioDir / s"trunk$trunkId/trunk_${trunkIdMap(trunkId)}-1-hilbert-curve.aif")
    val withFiles = pairs.flatMap { case (name, graph) =>
      val bncF = audioDir / "def_bnc" / s"$name.aif"
      if (!bncF.exists()) Some((name, graph, bncF)) else None
    }

    val futBnc = sequence(withFiles) { case (name, graph, bncF) =>
      println(s"Bouncing $name...")
      Evaluation.bounce(graph = graph, audioF = bncF, inputSpec = tempSpec)
    }

    futBnc.map(_ => ()).onComplete { tr =>
      println("Bounces finished.")
      println(tr)
      sys.exit()
    }
  }
}
