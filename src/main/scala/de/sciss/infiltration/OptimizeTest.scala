/*
 *  OptimizeTest.scala
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

import de.sciss.mellite.Mellite.executionContext
import de.sciss.mellite.{Application, Mellite}
import de.sciss.negatum.Optimize
import de.sciss.synth.SynthGraph

object OptimizeTest {
  def doubleWrapper(x: Double): Any = ()

  def main(args: Array[String]): Unit = {
    Application.init(Mellite)
    Mellite.initTypes()
    run()
  }

  def run(): Unit = {
    Optimize.MAX_MEM_SIZE_MB = 1
    Optimize.DEBUG = true

    val oCfg = Optimize.Config(
      graph         = gIn2,
      sampleRate    = 44100,
      analysisDur   = 5.9,
      blockSize     = 64,
      expandProtect = false, // true,
      expandIO      = false, // true,
    )
    val o = Optimize(oCfg)
    val t1 = System.currentTimeMillis()
    o.start()
    o.foreach { res =>
      val t2 = System.currentTimeMillis()
      println(s"Optimization found ${res.numConst} constant replacement and ${res.numEqual} redundant elements. Took ${t2 - t1}ms.")
      sys.exit()
    }
  }

  lazy val gIn1: SynthGraph = SynthGraph {
    import de.sciss.synth.ugen._
    import de.sciss.synth.{GE, _}
    val inf = Float.PositiveInfinity

    NegatumIn()
    val lFDClipNoise    = LFDClipNoise.ar(0.025048451)
    val min             = lFDClipNoise min 0.63839597
    val length          = Protect(2538.291, 3.0, 31.0, false)
    val median          = Median.ar(2211.6255, length = length)
    val freq_0          = Protect(median, -inf, inf, false)
    val lFDNoise3       = LFDNoise3.ar(freq_0)
    val lFDNoise1       = LFDNoise1.ar(2538.291)
    val iphase          = Protect(-554.53564, 0.0, 1.0, false)
    val width           = Protect(196.21611, 0.0, 1.0, false)
    val lFPulse         = LFPulse.ar(freq = 337.99133, iphase = iphase, width = width)
    val mix             = Mix(
      Seq[GE](min, lFDNoise3, 1.0, lFDNoise1, lFPulse))
    NegatumOut(mix)
  }

  lazy val gIn2: SynthGraph = SynthGraph {
    import de.sciss.synth.ugen._
    import de.sciss.synth.{GE, _}
    val inf = Float.PositiveInfinity

    NegatumIn()
    val in_0            = Protect(616.07404, -inf, inf, true)
    val bPZ2            = BPZ2.ar(in_0)
    val in_1            = Protect(83.6036, -inf, inf, true)
    val decay           = Decay.ar(in_1, time = 8.567501)
    val min_0           = decay min 0.05332149
    val min_1           = 0.05332149 min min_0
    val min_2           = min_1 min 0.05332149
    val min_3           = min_2 min 0.05332149
    val min_4           = 0.05332149 min min_3
    val in_2            = Protect(min_2, -inf, inf, true)
    val hPZ2            = HPZ2.ar(in_2)
    val min_5           = hPZ2 min 0.0
    val freq_0          = Protect(-1.9923483, 10.0, 60.0, false)
    val numHarm         = Protect(-0.010690896, 1.0, inf, false)
    val in_3            = Blip.ar(freq = freq_0, numHarm = numHarm)
    val maxDelayTime_0  = Protect(-1.9923483, 0.0, 20.0, false)
    val protect_0       = Protect(-1.9923483, 0.0, inf, false)
    val delayTime_0     = protect_0 min maxDelayTime_0
    val combL           = CombL.ar(in_3, maxDelayTime = maxDelayTime_0, delayTime = delayTime_0,
      decayTime = -0.018617123)
    val min_6           = hPZ2 min combL
    val in_4            = Protect(-0.018617123, -inf, inf, true)
    val lPZ1            = LPZ1.ar(in_4)
    val min_7           = lPZ1 min min_4
    val min_8           = min_7 min min_4
    val min_9           = min_8 min min_4
    val min_10          = min_9 min min_4
    val min_11          = min_10 min min_4
    val freq_1          = Protect(-0.010690896, 0.01, 20000.0, false)
    val iphase          = Protect(min_6, 0.0, 1.0, false)
    val width           = Protect(min_11, 0.0, 1.0, false)
    val lFPulse         = LFPulse.ar(freq = freq_1, iphase = iphase, width = width)
    val min_12          = min_5 min min_6
    val min_13          = min_12 min bPZ2
    val min_14          = min_13 min min_2
    val min_15          = min_6 min combL
    val min_16          = min_15 min combL
    val min_17          = min_16 min combL
    val min_18          = combL min min_17
    val min_19          = min_18 min min_17
    val min_20          = min_6 min min_18
    val min_21          = min_20 min min_2
    val min_22          = min_21 min min_2
    val min_23          = min_22 min min_2
    val in_5            = Protect(0.9535084, -inf, inf, true)
    val maxDelayTime_1  = Protect(min_2, 0.0, 20.0, false)
    val protect_1       = Protect(min_23, 0.0, inf, false)
    val delayTime_1     = protect_1 min maxDelayTime_1
    val delayN          = DelayN.ar(in_5, maxDelayTime = maxDelayTime_1, delayTime = delayTime_1)
    val leq             = delayN <= 3917.4714
    val decayTime_0     = leq min min_19
    val in_6            = Protect(-1.9923483, -inf, inf, true)
    val maxDelayTime_2  = Protect(min_4, 0.0, 20.0, false)
    val protect_2       = Protect(min_11, 0.0, inf, false)
    val delayTime_2     = protect_2 min maxDelayTime_2
    val combC           = CombC.ar(in_6, maxDelayTime = maxDelayTime_2, delayTime = delayTime_2,
      decayTime = decayTime_0)
    val min_24          = combC min 0.42587343
    val min_25          = min_24 min 0.42587343
    val min_26          = min_25 min 0.42587343
    val min_27          = min_19 min min_17
    val min_28          = min_27 min min_17
    val min_29          = min_4 min min_20
    val min_30          = min_2 min 0.05332149
    val min_31          = min_30 min 0.05332149
    val roundTo         = in_3 roundTo 8.567501
    val freq_2          = Protect(min_6, -inf, inf, false)
    val lFDNoise3       = LFDNoise3.ar(freq_2)
    val in_7            = Protect(min_2, -inf, inf, true)
    val freq_3          = Protect(min_15, 10.0, 20000.0, false)
    val lPF             = LPF.ar(in_7, freq = freq_3)
    val min_32          = combL min -0.010690896
    val distort         = min_32.distort
    val min_33          = min_32 min combL
    val in_8            = Protect(lPF, -inf, inf, true)
    val freq_4          = Protect(min_33, 10.0, 20000.0, false)
    val rq              = Protect(min_31, 0.01, 100.0, false)
    val bRF             = BRF.ar(in_8, freq = freq_4, rq = rq)
    val freq_5          = Protect(3917.4714, 0.1, 60.0, false)
    val phase           = Protect(min_21, 0.0, 1.0, false)
    val impulse         = Impulse.ar(freq = freq_5, phase = phase)
    val in_9            = Protect(0.0033084024, -inf, inf, true)
    val freq_6          = Protect(min_3, 10.0, 20000.0, false)
    val hPF             = HPF.ar(in_9, freq = freq_6)
    val in_10           = Protect(0.0033084024, -inf, inf, true)
    val freq_7          = Protect(min_16, -20000.0, 20000.0, false)
    val freqShift       = FreqShift.ar(in_10, freq = freq_7, phase = 0.9535084)
    val wrap2           = freqShift wrap2 roundTo
    val mix             = Mix(
      Seq[GE](lFPulse, min_14, min_26, min_28, min_29, lFDNoise3, distort, bRF, impulse, hPF, wrap2))
    NegatumOut(mix)
  }

  // this is the result, which is wrong.
  // the DC calculation is wrong. It should be 1.0 + 0.64,
  // but somehow it includes
  lazy val gOut: SynthGraph = SynthGraph {
    import de.sciss.synth.GE
    import de.sciss.synth.ugen._

    NegatumIn()
    val lFDNoise1 = LFDNoise1.ar(2538.291)
    val lFDNoise3 = LFDNoise3.ar(2211.6255)
    val iphase    = Protect(-554.53564, 0.0, 1.0, false)
    val width     = Protect(196.21611, 0.0, 1.0, false)
    val lFPulse   = LFPulse.ar(freq = 337.99133, iphase = iphase, width = width)
    val dC        = DC.ar(2244.264)
    val mix       = Mix(Seq[GE](lFDNoise1, lFDNoise3, lFPulse, dC))
    NegatumOut(mix)
  }
}
