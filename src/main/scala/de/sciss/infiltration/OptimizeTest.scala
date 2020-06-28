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
    val oCfg = Optimize.Config(
      graph         = gIn,
      sampleRate    = 44100,
      analysisDur   = 5.9,
      blockSize     = 64,
      expandProtect = false, // true,
      expandIO      = false, // true,
    )
    val o = Optimize(oCfg)
    o.start()
    o.foreach { res =>
      println(s"Optimization found ${res.numConst} constant replacement and ${res.numEqual} redundant elements.")
      sys.exit()
    }
  }

  val gIn: SynthGraph = SynthGraph {
    import de.sciss.synth.{GE, _}
    import de.sciss.synth.ugen._
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

  // this is the result, which is wrong.
  // the DC calculation is wrong. It should be 1.0 + 0.64,
  // but somehow it includes
  val gOut: SynthGraph = SynthGraph {
    import de.sciss.synth.{GE, _}
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
