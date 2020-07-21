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
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.{Buffer, Server, Synth, Txn}
import de.sciss.mellite.Mellite
import de.sciss.numbers
import de.sciss.osc.Message
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{AuralSystem, Durable, Grapheme, Runner, SoundProcesses, Universe, Workspace}
import de.sciss.synth.{SynthGraph, addToTail, message}

import scala.concurrent.stm.Ref
import scala.concurrent.stm.Txn.afterRollback

object PlayChain {
  def any2stringadd(in: Any): Any = ()

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
//    val sch = u.scheduler

//    val procIdxR  = Ref(247)
//    val procIdxR = Ref(256)
//    val procIdxR = Ref(257)
//    val procIdxR = Ref(259)
//    val procIdxR = Ref(271)
//    val procIdxR = Ref(280)
//    val procIdxR = Ref(285)
    val procIdxR = Ref(291)

//    val runnerR   = Ref(List.empty[Runner[S]])

//    def play()(implicit tx: S#Tx): Unit = {
//      val g     = circleH()
//      val procIdx = procIdxR.getAndTransform(_ + 1)
//      if (procIdx < numProcs) {
//        val proc  = g.at(procIdx).getOrElse(sys.error("Woopa")).value
//        println(s"play($procIdx) = ${proc.name}")
//        val r     = Runner(proc)
//        r.run()
//        val rL = r :: runnerR()
//        val (rKeep, rFree) = rL.splitAt(4)
//        runnerR() = rKeep
//        rFree.foreach(_.dispose())
//        sch.schedule(sch.time + (TimeRef.SampleRate * 3).toLong) { implicit tx =>
//          play()
//        }
//      } else {
//        tx.afterCommit {
//          println("Und tschüß")
//          wsIn.close()
//          sys.exit()
//        }
//      }
//    }

    def play(server: Server)(implicit tx: S#Tx): Unit = {
      val g     = circleH()
      val procIdx = procIdxR()

      val proc  = g.at(procIdx).getOrElse(sys.error("Woopa")).value
      println(s"play($procIdx) = ${proc.name}")
      val r     = Runner(proc)
      r.run()

      val gPeak = SynthGraph {
        import de.sciss.synth.ugen._
        val sig   = Mix.mono(In.ar(0, 4))
        val tr    = Impulse.kr(0.5)
        val peak  = Peak.kr(sig, tr)
//        peak.ampDb.poll(tr, "peak")
        val trS   = tr - Impulse.kr(0)  // ignore single initial peak
        SendReply.kr(trS, peak, "/$meter")
      }
      val synPeak = Synth.play(gPeak)(target = server.defaultGroup, addAction = addToTail)
      val SynPeakId = synPeak.peer.id
//      val gainR = Ref(1.0)

//      val peakCount = Ref(0)

      lazy val respPeak: message.Responder = message.Responder.add(synPeak.server.peer) {
        case Message("/$meter", SynPeakId, _, vals @ _*) =>
          val pairs = vals.asInstanceOf[Seq[Float]].toIndexedSeq
          val peak  = pairs.head
          import numbers.Implicits._
          println(s"peak = ${peak.ampDb} dB")
          if (peak < -30.dbAmp) {
//            s.peer.dumpTree(true)
            cursor.step { implicit tx =>
              val r = math.random()
//              val g = gainR.transformAndGet(_ * 1.5)
//              server.defaultGroup.set(s"$$at_gain" -> g)
              val parIdx = if (math.random() < 0.5) 1 else 2
              server.defaultGroup.set(s"$$at_p$parIdx" -> Vector.fill(4)(r.toFloat))
            }
          } else {
            cursor.step { implicit tx =>
              synPeak.free()
            }
            respPeak.remove()
          }
      }
      respPeak
      afterRollback(_ => respPeak.remove())(tx.peer)
      synPeak.onEnd(respPeak.remove())

      val gLoud = SynthGraph {
        import de.sciss.synth.ugen._
        import de.sciss.synth.Ops.stringToControl
        val mG0   = "main-gain".kr(1.0)
        val mG    = Lag.kr(mG0, 10.0f)
        val in    = In.ar(0, 4)
        val sig   = Mix.mono(in)
        val sigM  = (sig * mG).clip2(1.0) // .max(-1.0).min(1.0)
//        CheckBadValues.ar(sigM)
        Lag.kr(sigM.abs.ampDb, 1.0).poll(2, "lag")
        val b     = LocalBuf(1024, 1)
        val fft   = FFT(b, HPF.ar(sigM, 100), /*hop = 1.0,*/ winType = 1)
        val loud  = Loudness.kr(fft) // , tmask = 6.0)
        val flat0 = SpecFlatness.kr(fft)
//        val flat1 = CheckBadValues.kr(flat0, post = 0)
//        val flat  = Gate.kr(flat0, flat1 sig_== 0)
        val flat = flat0.clip(0.0, 0.5)
//        loud.poll(3)
        Lag.kr(flat, 1.0).poll(1, "flat")
        val tr    = Impulse.kr(0.5)
//        val peak  = Peak.kr(loud / flat.max(0.01), tr)
//        val peak  = Peak.kr(loud * (2.0 - flat).pow(1.41), tr)
//        val peak  = Peak.kr(loud + (1.0 - flat) * 40, tr)
        val peak  = Peak.kr(loud + flat/*.clip(0.0, 0.5)*/.linLin(0.0, 0.5, 36, 0.0), tr)
        val trS   = tr - Impulse.kr(0)  // ignore single initial peak
        SendReply.kr(trS, peak, "/$loud")
//        ReplaceOut.ar(0, Limiter.ar(in * mG))
        val inG = in * mG
        ReplaceOut.ar(0, Limiter.ar(Seq(inG.out(0) + inG.out(2), inG.out(1) + inG.out(3))))
      }
      val synLoud = Synth.play(gLoud)(target = server.defaultGroup, addAction = addToTail, dependencies = /*bLoud ::*/ Nil)
      val SynLoudId = synLoud.peer.id

      val mainGainR = Ref(1.0)

      lazy val respLoud: message.Responder = message.Responder.add(synLoud.server.peer) {
        case Message("/$loud", SynLoudId, _, vals @ _*) =>
          val pairs = vals.asInstanceOf[Seq[Float]].toIndexedSeq
          val loud  = pairs.head
          import numbers.Implicits._
          println(s"loud = $loud")
          if (loud < 60) {
            cursor.step { implicit tx =>
              val g = mainGainR.transformAndGet(_ * 1.0.dbAmp)
              if (g < 36.dbAmp) {
                server.defaultGroup.set("main-gain" -> g)
              }
            }
          } else if (loud > 60) {
            cursor.step { implicit tx =>
              val g = mainGainR.transformAndGet(_ * (-2.0).dbAmp)
              if (g > -36.dbAmp) {
                server.defaultGroup.set("main-gain" -> g)
              }
            }
          }
      }
      respLoud
      afterRollback(_ => respLoud.remove())(tx.peer)
      synLoud.onEnd(respLoud.remove())

      //      val rL = r :: runnerR()
//      val (rKeep, rFree) = rL.splitAt(4)
//      runnerR() = rKeep
//      rFree.foreach(_.dispose())
//      sch.schedule(sch.time + (TimeRef.SampleRate * 3).toLong) { implicit tx =>
//        play()
//      }
    }

    cursor.step { implicit tx =>
      val as = Mellite.auralSystem
      as.start()
      as.addClientNow(new AuralSystem.Client {
        def auralStarted(server: Server)(implicit tx: Txn): Unit = {
          tx.afterCommit {
            server.peer.dumpOSC()
            SoundProcesses.step[S]("start") { implicit tx =>
              play(server)
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
