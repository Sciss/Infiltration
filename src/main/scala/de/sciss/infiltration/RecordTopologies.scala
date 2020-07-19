/*
 *  RecordTopologies.scala
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
import de.sciss.lucre.synth.InMemory
import de.sciss.nuages.{DSL, ExpWarp, IntWarp, Nuages, ParamSpec, ParametricWarp, ScissProcs, Util, Wolkenpumpe, WolkenpumpeMain, LinearWarp => LinWarp}
import de.sciss.submin.Submin
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.graph.ScanInFix
import de.sciss.synth.proc.{Proc, Universe}
import de.sciss.synth.ugen.{ControlValues, LinXFade2}
import de.sciss.synth.{GE, Server, proc}

import scala.swing.{Button, Swing}

object RecordTopologies {
  def main(args: Array[String]): Unit = {
    Wolkenpumpe.init()
    Swing.onEDT {
      Submin.install(true)
      run()
    }
  }

  def dumpTopology[S <: Sys[S]](n: Nuages[S])(implicit tx: S#Tx): Unit = {
    n.surface match {
      case Nuages.Surface.Folder(f) =>
        f.iterator.foreach {
          case p: Proc[S] =>
            println(p.name)

          case other =>
            println(s"(ignoring $other)")
        }

      case Nuages.Surface.Timeline(_) =>
        sys.error("Timeline not supported")
    }
  }

  def any2stringadd(in: Any): Any = ()

  def run(): Unit = {
    type S = InMemory
    implicit val system: S = InMemory()
    val w: WolkenpumpeMain[S] = new WolkenpumpeMain[S] {
      override protected def configure(sCfg: ScissProcs.ConfigBuilder, nCfg: Nuages.ConfigBuilder,
                                       aCfg: Server.ConfigBuilder): Unit = {
        super.configure(sCfg, nCfg, aCfg)
        sCfg.genNumChannels = 4
        nCfg.mainChannels   = Some(0 until 4)
        nCfg.soloChannels   = None
        nCfg.lineInputs     = Vector.empty
        nCfg.lineOutputs    = Vector.empty
      }

      override protected def registerProcesses(nuages: Nuages[S], nCfg: Nuages.Config, sCfg: ScissProcs.Config)
                                              (implicit tx: S#Tx, universe: Universe[S]): Unit = {
        super.registerProcesses(nuages, nCfg, sCfg)

        val dsl = DSL[S]
        import dsl._
        import sCfg.genNumChannels

        val mainChansOption = nCfg.mainChannels

        def ForceChan(in: GE): GE = if (genNumChannels <= 0) in else {
          Util.wrapExtendChannels(genNumChannels, in)
        }

        implicit val _nuages: Nuages[S] = nuages

        def filterF   (name: String)(fun: GE => GE): proc.Proc[S] =
          filter      (name, if (DSL.useScanFixed) genNumChannels else -1)(fun)

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
      }
    }

    val nuagesH = system.step { implicit tx => tx.newHandle(Nuages.folder[S]) }
    w.run(nuagesH)

    w.view.addSouthComponent(Button("Dump Topology") {
      system.step { implicit tx =>
        dumpTopology(nuagesH())
      }
    })
  }
}
