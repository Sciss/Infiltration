/*
 *  Infiltration.scala
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

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth.{InMemory, Server, Synth, Txn}
import de.sciss.nuages.{DSL, ExpWarp, IntWarp, NamedBusConfig, Nuages, NuagesAttribute, NuagesPanel, ParamSpec, ParametricWarp, ScissProcs, Util, Wolkenpumpe, WolkenpumpeMain, LinearWarp => LinWarp}
import de.sciss.synth.proc.{AuralSystem, Universe}
import de.sciss.synth.ugen.{CheckBadValues, ControlValues, Gate, HPF, LinXFade2, Out}
import de.sciss.synth.{GE, SynthGraph, addAfter, proc, Server => SServer}

import scala.swing.{Button, Swing}

object Infiltration {
  def main(args: Array[String]): Unit = {
    Wolkenpumpe.init()
    Swing.onEDT {
      run()
    }
  }

  def dumpTopology[S <: Sys[S]](np: NuagesPanel[S])(implicit tx: S#Tx): Unit = {
    val n = np.nodes
    n.foreach { p =>
      println(p.name)
      p.attributes.foreach { case (key, a) =>
        val vec = a.inputView match {
          case nm: NuagesAttribute.Numeric => Some(nm.numericValue)
          case _ => None
        }
        println(s"  < $key: ${Option(a.numericValue)} - ${a.spec} - $vec")
      }
      p.outputs.foreach { case (key, a) =>
        println(s"  > $key: ${a.mappings}")
      }
    }

    //    n.surface match {
    //      case Nuages.Surface.Folder(f) =>
    //        f.iterator.foreach {
    //          case p: Proc[S] =>
    //
    //            println(p.name)
    //
    //          case other =>
    //            println(s"(ignoring $other)")
    //        }
    //
    //      case Nuages.Surface.Timeline(_) =>
    //        sys.error("Timeline not supported")
    //    }
  }

  def any2stringadd(in: Any): Any = ()

  def run(): Unit = {
    type S = InMemory
    implicit val system: S = InMemory()
    val w: WolkenpumpeMain[S] = new WolkenpumpeMain[S] {
      override protected def configure(sCfg: ScissProcs.ConfigBuilder, nCfg: Nuages.ConfigBuilder,
                                       aCfg: SServer.ConfigBuilder): Unit = {
        super.configure(sCfg, nCfg, aCfg)
        sCfg.genNumChannels = 4
        nCfg.mainChannels   = Some(0 until 4)
        nCfg.soloChannels   = None
        // we need one here so that the right number of input channels is chosen
        nCfg.lineInputs     = Vector(NamedBusConfig("ignore", 0 until 2))
        nCfg.lineOutputs    = Vector.empty
        nCfg.micInputs      = Vector.empty
        nCfg.mainSynth      = false
        aCfg.deviceName     = Some("Infiltration")
      }

      override protected def registerProcesses(nuages: Nuages[S], nCfg: Nuages.Config, sCfg: ScissProcs.Config)
                                              (implicit tx: S#Tx, universe: Universe[S]): Unit = {
        super.registerProcesses(nuages, nCfg, sCfg)

        val dsl = DSL[S]
        import dsl._
        import sCfg.genNumChannels

        //        val mainChansOption = nCfg.mainChannels

        def ForceChan(in: GE): GE = if (genNumChannels <= 0) in else {
          Util.wrapExtendChannels(genNumChannels, in)
        }

        implicit val _nuages: Nuages[S] = nuages

        def filterF   (name: String)(fun: GE => GE): proc.Proc[S] =
          filter      (name, if (DSL.useScanFixed) genNumChannels else -1)(fun)

        def collectorF(name: String)(fun: GE => Unit): proc.Proc[S] =
          collector   (name, if (DSL.useScanFixed) genNumChannels else -1)(fun)

        def mix(in: GE, flt: GE, mix: GE): GE = LinXFade2.ar(in, flt, mix * 2 - 1)
        def mkMix(df: Double = 0.0): GE = pAudio("mix", ParamSpec(0, 1), default(df))

        def default(in: Double): ControlValues =
          if (genNumChannels <= 0)
            in
          else
            Vector.fill(genNumChannels)(in)

        generator("negatum-3f704ff8") {
          import de.sciss.synth.ugen._

          //          shortcut = "N"
          val freq_0  = pAudio("p1", ParamSpec( 13.122, 1.73999104e8, ExpWarp), default(1.73999104e8))
          val freq_1  = pAudio("p2", ParamSpec(-13.122, 22050.0     , LinWarp), default( 0.14800115))
          val in_6    = pAudio("p3", ParamSpec( -0.81 ,     2.05    , LinWarp), default(-0.29881296))
          val param   = pAudio("p4", ParamSpec( -0.32 , 22050.0     , LinWarp), default( 0.0))

          RandSeed.ir(trig = 1, seed = 56789.0)
          val in_0            = Impulse.ar(freq = 60.0, phase = 1.0)
          val bPZ2            = BPZ2.ar(in_0)
          val lFDNoise1_0     = LFDNoise1.ar(freq_0)
          val in_1            = lFDNoise1_0 min param
          val in_2            = LeakDC.ar(in_1, coeff = 0.995)
          val in_3            = Delay1.ar(in_2)
          val lag             = Lag.ar(in_3, time = 1.0)
          val in_4            = param min in_3
          val min_0           = in_4 min lag
          val min_1           = in_1 min min_0
          val in_5            = LeakDC.ar(in_4, coeff = 0.995)
          val delay1          = Delay1.ar(in_5)
          val min_2           = bPZ2 min param
          val impulse         = Impulse.ar(freq = 0.1, phase = 1.0)
          val lFDNoise1_1     = LFDNoise1.ar(freq_1)
          val dC              = DC.ar(in_6)
          val in_7            = Mix(Seq[GE](min_1, delay1, min_2, impulse, lFDNoise1_1, dC))
          val checkBadValues  = CheckBadValues.ar(in_7, id = 0.0, post = 0.0)
          val gate_0          = checkBadValues sig_== 0.0
          val gate_1          = Gate.ar(in_7, gate = gate_0)
          val in_8            = gate_1 clip2 1.0
          val leakDC          = LeakDC.ar(in_8, coeff = 0.995)
          val times           = leakDC * 0.47
          times
        }

        generator("negatum-8bbebbf4") {
          import de.sciss.synth.ugen._

          shortcut = "N"
          val in_0    = pAudio("p1", ParamSpec( 10.0, 60.0, ExpWarp), default(60.0))
          val param_2 = pAudio("p2", ParamSpec( -22050.0, 22050.0, LinWarp), default(2.0))
          val param_0 = pAudio("p3", ParamSpec( 1.0, 22050.0, ExpWarp), default(2885.5125))
          val param_1 = pAudio("p4", ParamSpec( -0.127, 22050.0, LinWarp), default(0.008551382))
          val in_4    = pAudio("p5", ParamSpec( -0.05, 2.05, LinWarp), default(0.0))

          RandSeed.ir(trig = 1, seed = 56789.0)
          val freq            = Clip.ar(in_0, lo = 10.0, hi = 60.0)
          val numHarm         = param_0 max 1.0
          val blip            = Blip.ar(freq = freq, numHarm = numHarm)
          val in_1            = param_1 min blip
          val min_0           = in_1 min 8.9271076E-4
          val in_2            = LeakDC.ar(in_1, coeff = 0.995)
          val in_3            = BRZ2.ar(in_2)
          val min_1           = in_4 min in_4
          val maxDelayTime    = Clip.ar(in_4, lo = 0.01, hi = 20.0)
          val max             = min_1 max 0.0
          val delayTime       = max min maxDelayTime
          val in_5            = DelayC.ar(in_3, maxDelayTime = maxDelayTime, delayTime = delayTime)
          val leakDC_0        = LeakDC.ar(in_5, coeff = 0.8)
          val in_6            = leakDC_0 roundTo in_5
          val neq             = in_4 sig_!= in_6
          val in_7            = in_4 min in_6
          val in_8            = LeakDC.ar(in_7, coeff = 0.995)
          val hPZ1_0          = HPZ1.ar(in_8)
          val min_2           = in_7 min min_0
          val dC              = DC.ar(0.001)
          val slew            = Slew.ar(in_7, up = 0.001, down = 0.001)
          val times_0         = in_7 * param_2
          val in_9            = leakDC_0.distort
          val hPZ1_1          = HPZ1.ar(in_9)
          val phase           = Clip.ar(in_6, lo = 0.0, hi = 1.0)
          val impulse         = Impulse.ar(freq = 0.1, phase = phase)
          val min_3           = (-0.12408662: GE) min blip
          val in_10           = Mix(Seq[GE](neq, hPZ1_0, min_2, dC, slew, times_0, hPZ1_1, impulse, min_3))
          val checkBadValues  = CheckBadValues.ar(in_10, id = 0.0, post = 0.0)
          val gate_0          = checkBadValues sig_== 0.0
          val gate_1          = Gate.ar(in_10, gate = gate_0)
          val in_11           = gate_1 clip2 1.0
          val leakDC_1        = LeakDC.ar(in_11, coeff = 0.995)
          val times_1         = leakDC_1 * 0.47
          times_1
        }

        filterF("adapt") { in =>
          import de.sciss.synth.ugen._

          val pLo   = pAudio("lo"   , ParamSpec(0.0, 1.0), default(0.0))
          val pHi   = pAudio("hi"   , ParamSpec(0.0, 1.0), default(1.0))
          val pRect = pAudio("abs"  , ParamSpec(0.0, 1.0, IntWarp), default(0.0))
          val pGain = pAudio("gain" , ParamSpec(-30, 30), default(0.0))
          val pMix  = mkMix()
          val inG   = in * pGain.dbAmp
          val flt1  = inG     .linLin(-1.0, 1.0, pLo, pHi)
          val flt2  = inG.abs .linLin( 0.0, 1.0, pLo, pHi)
          val flt   = Select.ar(pRect, flt1 :: flt2 :: Nil)
          mix(in, flt, pMix)
        }

        filterF("s+hF") { in =>
          import de.sciss.synth.ugen._

          val pFreq   = pAudio("freq" , ParamSpec(0.00, 100.0, ParametricWarp(10.0)), default(1.0))
          val pMode   = pAudio("mode" , ParamSpec(0.0, 1.0, IntWarp), default(0.0))
          val pMix  = mkMix()
          val trig    = Impulse.ar(pFreq)
          val latch   = Latch.ar(in, trig)
          val tt      = Timer.ar(trig)
          val eg      = DemandEnvGen.ar(latch, tt)
          val flt     = Select.ar(pMode, latch :: eg :: Nil)
          mix(in, flt, pMix)
        }

        filterF("s+hT") { in =>
          import de.sciss.synth.ugen._

          //          val pTrig   = pAudioIn("in_t")
          val pTrig   = pAudio("trig" , ParamSpec(0.0, 1.0), default(0.0))
          val pMode   = pAudio("mode" , ParamSpec(0.0, 1.0, IntWarp), default(0.0))
          val pMix    = mkMix()
          val trig    = pTrig
          val latch   = Latch.ar(in, trig)
          val tt      = Timer.ar(trig)
          val eg      = DemandEnvGen.ar(latch, tt)
          val flt     = Select.ar(pMode, latch :: eg :: Nil)
          mix(in, flt, pMix)
        }

        generator("in") {
          import de.sciss.synth.ugen._
          val pBoost  = pAudio("gain", ParamSpec(0.1, 10, ExpWarp), default(1.0))
          val pBal    = pAudio("$bal", ParamSpec(-1.0, 1.0, LinWarp), default(0.0))
          val sig0    = In.ar(NumOutputBuses.ir, 2)
          val sig     = LeakDC.ar(sig0)
          val sigL    = sig.out(0)
          val sigR    = sig.out(1)
          val balance = Balance2.ar(sigL, sigR, pos = pBal, level = pBoost)
          val sum     = balance.left + balance.right
          val buf     = LocalBuf(1024, 1) // WARNING: must be 1024 for Loudness
          val fft     = FFT(buf, sum, hop = 1.0, winType = 1)
          val loud    = Loudness.kr(fft)
          loud.poll(1, "loud")
          val sig1: GE = ForceChan(sum)
          sig1
        }

        def mkDirectOut(sig0: GE): Unit = {
          val bad = CheckBadValues.ar(sig0)
          val sig = Gate.ar(sig0, bad sig_== 0)
          nCfg.mainChannels.zipWithIndex.foreach { case (ch, i) =>
            val sig0 = sig out i
            val hpf  = sCfg.highPass
            val sig1 = if (hpf >= 16 && hpf < 20000) HPF.ar(sig0, hpf) else sig0
            Out.ar(ch, sig1)   // XXX TODO - should go to a bus w/ limiter
          }
        }

        collectorF("O-inf") { in =>
          import de.sciss.synth.ugen._
          //          val in0 = In.ar(0, 4)
          val inM = Mix.mono(in)
          CheckBadValues.ar(inM)
          val b1  = BPF.ar(inM, freq =  333, rq = 1)
          val b2  = BPF.ar(inM, freq = 1000, rq = 1)
          val b3  = BPF.ar(inM, freq = 3000, rq = 1)
          val r1  = Decay.kr(b1.abs, 0.2)
          val r2  = Decay.kr(b2.abs, 0.2)
          val r3  = Decay.kr(b3.abs, 0.2)
          //          r1.ampDb.poll(1, "r1")
          //          r2.ampDb.poll(1, "r2")
          //          r3.ampDb.poll(1, "r3")
          val gainF = ((r1 + 3 * r2 + 2 * r3).reciprocal * 10).min(8.0 /*4.0*/)
          val gain = LagUD.kr(gainF, timeUp = 2.0, timeDown = 0.1)
          //val gain = Lag.kr(gainF, 1.0).min(4.0)
          //          gain.ampDb.poll(1, "gain")
          val inG = in * gain
          val sig = inG // Limiter.ar(inG)
          //          ReplaceOut.ar(0, sig) // in0 /* DelayN.ar(in0, 0.2, 0.2) */ * gain)
          //          Out.ar(0, sig)
          mkDirectOut(sig)
          //r1.poll(1, "r1")
          //r2.poll(1, "r2")
        }
      }
    }

    val nuagesH = system.step { implicit tx =>
      tx.newHandle(Nuages.folder[S])
    }
    w.run(nuagesH)
    val view  = w.view
    val panel = view.panel
    panel.display.setHighQuality(false)

    view.addSouthComponent(Button("Top") {
      system.step { implicit tx =>
        dumpTopology(w.view.panel)
      }
    })

    view.addSouthComponent(Button("Sta") {
      val cOpt = system.step { implicit tx =>
        w.auralSystem.serverOption.map { s =>
          s.counts
        }
      }
      cOpt.foreach(println)
    })

    system.step { implicit tx =>
      w.auralSystem.addClientNow(new AuralSystem.Client {
        def auralStarted(server: Server)(implicit tx: Txn): Unit = {
          val dfPostM = SynthGraph {
            import de.sciss.synth._
            import de.sciss.synth.ugen._
//            val nConfig     = panel.config
            import Ops._
            val amp   = "amp".kr(1f)
            val in0   = In.ar(0, 4)
            val in  = Mix.mono(in0)
            CheckBadValues.ar(in)
            val b1  = BPF.ar(in, freq =  333, rq = 1)
            val b2  = BPF.ar(in, freq = 1000, rq = 1)
            val b3  = BPF.ar(in, freq = 3000, rq = 1)
            val r1  = Decay.kr(b1.abs, 0.2)
            val r2  = Decay.kr(b2.abs, 0.2)
            val r3  = Decay.kr(b3.abs, 0.2)
//            r1.ampDb.poll(1, "r1")
//            r2.ampDb.poll(1, "r2")
//            r3.ampDb.poll(1, "r3")
            val gainF = ((r1 + 3 * r2 + 2 * r3).reciprocal * 10).min(8.0 /*4.0*/)
            val gain = LagUD.kr(gainF, timeUp = 2.0, timeDown = 0.1) * amp
//            gain.ampDb.poll(1, "gain")
            val sig = Limiter.ar(in0 * gain)
            ReplaceOut.ar(0, sig)
          }
          val synPostM = Synth.play(dfPostM, Some("post-main"))(server.defaultGroup, addAction = addAfter)
          panel.mainSynth = Some(synPostM)
        }

        def auralStopped()(implicit tx: Txn): Unit = ()
      })
    }
  }
}
