/*
 *  RunNegatum.scala
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
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth.InMemory
import de.sciss.mellite.{Application, Mellite}
import de.sciss.negatum.{Negatum, Rendering}
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{AudioCue, Universe}
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

object RunNegatum {
  final case class Config(template: File, startFrame: Int = 1, endFrame: Int = -1) {
    def formatTemplate(frame: Int): File = {
      template.replaceName(template.name.format(frame))
    }
  }

  def main(args: Array[String]): Unit = {
    // File.createTemp(deleteOnExit = false)
    object p extends ScallopConf(args) {
      printedName = "in|filtration"

      val template: Opt[File] = opt(required = true,
        descr = "Template input sound file where %d is the frame place holder"
      )
      val startFrame: Opt[Int] = opt("start-frame", validate = _ >= 0, default = Some(1),
        descr = "Start frame index"
      )
      val endFrame: Opt[Int] = opt("end-frame", required = true, validate = _ >= 0,
        descr = "End frame index (inclusive)"
      )

      verify()
      val config: Config = Config(
        template = template(),
        startFrame = startFrame(),
        endFrame = endFrame()
      )
    }

    type S = InMemory
    implicit val system: S = InMemory()
    implicit val universe: Universe[S] = system.step { implicit tx => Universe.dummy[S] }
    run[S](p.config)
  }

  def run[S <: Sys[S]](config: Config)(implicit universe: Universe[S]): Unit = {
    Application.init(Mellite)
    Mellite.initTypes()
//    Negatum.init()

    val fStart    = config.formatTemplate(config.startFrame)
    val specStart = AudioFile.readSpec(fStart)
    val sync      = new AnyRef

    println("_" * 100)

    /*val rendering =*/ universe.cursor.step { implicit tx =>
      val cueStart    = AudioCue(fStart, specStart, offset = 0L, gain = 1.0)
      val cueStartObj = AudioCue.Obj.newConst[S](cueStart)

      val n = Negatum[S](cueStartObj)
      val generation  = Negatum.Generation(
        population      = 10, // 1000,
        probConst       = 0.5,
        minVertices     = 32,
        maxVertices     = 128,
        probDefault     = 0.05,
        allowedUGens    = Set.empty, // XXX TODO
      )
      val evaluation  = Negatum.Evaluation(
        minFreq         = 100,
        maxFreq         = 16000,
        numMel          = 42,
        numMFCC         = 32,
        maxBoost        = 10.0,
        timeWeight      = 0.5,
      )
      val penalty     = Negatum.Penalty()
      val breeding    = Negatum.Breeding(
        selectFraction  = 0.33,
        elitism         = 3,
        minMut          = 2,
        maxMut          = 5,
        probMut         = 0.75,
        golem           = 15
      )
      val nCfg        = Negatum.Config(
        seed            = System.currentTimeMillis(),
        generation      = generation,
        evaluation      = evaluation,
        penalty         = penalty,
        breeding        = breeding
      )

      val r   = n.run(nCfg)
      val nH  = tx.newHandle(n)

      var progressLast = 0
      r.reactNow { implicit tx => {
        case Rendering.Progress(amt) =>
          tx.afterCommit {
            val n = (amt * 100).toInt
            while (progressLast < n) {
              print('#')
              progressLast += 1
            }
          }

        case Rendering.Completed(tr) =>
          if (tr.isSuccess) {
            val n = nH()
            // println(s"\nPopulation.size: ${n.population.size}")
          }

          tx.afterCommit {
            println()
            println(tr)
            sync.synchronized {
              sync.notifyAll()
            }
          }

        case _ =>
      }
      }

      () // r
    }

    new Thread {
      override def run(): Unit =
        sync.synchronized {
          sync.wait()
          println("Done.")
          sys.exit()
        }

      start()
    }
  }
}
