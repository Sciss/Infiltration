/*
 *  Parametrize.scala
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

import de.sciss.kollflitz.Vec
import de.sciss.negatum.Vertex
import de.sciss.negatum.impl.{Chromosome, MkSynthGraph, MkTopology}
import de.sciss.synth.proc.graph.Attribute
import de.sciss.synth.proc.impl.MkSynthGraphSource
import de.sciss.synth.{GE, SynthGraph, UGenSpec, audio}

object Parametrize {
  def main(args: Array[String]): Unit = {
    run()
  }

  final case class ControlVertex(name: String, values: Vec[Float]) extends Vertex.UGen {
    val info: UGenSpec = UGenSpec(
      name        = "AudioControl",
      attr        = Set.empty,
      rates       = UGenSpec.Rates.Implied(audio, UGenSpec.RateMethod.Default),
      args        = Vector.empty,
      inputs      = Vector.empty,
      outputs     = Vector.tabulate(values.size) { _ =>
        UGenSpec.Output(name = None, shape = UGenSpec.SignalShape.Generic, variadic = None)
      },
      doc         = None,
      elemOption  = None,
    )

    def instantiate(ins: Vec[(AnyRef, Class[_])]): GE =
      Attribute.ar(name, values)

    def copy(): Vertex = new ControlVertex(name = name, values = values)
  }

  def run(): Unit = {
    val _gIn      = gIn2
    val topIn     = MkTopology(_gIn)
    val numConst  = topIn.vertices.count(_.isConstant)
    val constants = topIn.vertices.collect {
      case vc: Vertex.Constant => vc
    }
    println(s"Num.constants $numConst")
    println(constants.map(_.f))

    val topOut = constants.zipWithIndex.foldLeft(topIn) { case (topAcc, (vc, idx)) =>
      val vCtl = ControlVertex(s"ctl_$idx", Vector(vc.f))
      val topAdd = topAcc.addVertex(vCtl)
      Chromosome.replaceVertex(topAdd, vOld = vc, vNew = vCtl)
    }

    val _gOut   = MkSynthGraph(topOut)
    val srcOut  = MkSynthGraphSource(_gOut)
    println(srcOut)
  }

  /*

    observations

    - we can plug into constants
    - discount places where only scalar values or only dynamic values are accepted
    - we can make use of the `Mix` balance
    - what do we do with the particular DC component in the mix?

    we can thus determine the maximum number of parameters, and should perhaps
    rank them according to "audible" effect. Which is

    - a particular form of decorrelation as we tune the parameter
    - we need to avoid that the sound becomes, for example, silent

    What do we do with special cases like `GVerb` which produces multiple outputs?

    From the topology, we get single constant vertices, even if the constant
    is used multiple times. We have to decide whether to split that vertex or not.

   */
  val gIn1: SynthGraph = SynthGraph {
    import de.sciss.synth.GE
    import de.sciss.synth.ugen._

    // negatum-b82d9b9e-opt
    NegatumIn()
    val lFDNoise3_0 = LFDNoise3.ar(1.0)
    val lFDNoise3_1 = LFDNoise3.ar(-0.088856705)
    val lFPulse     = LFPulse.ar(freq = 20000.0, iphase = 1.0, width = 0.0)
    val impulse     = Impulse.ar(freq = 60.0, phase = 0.0010397598)
    val dC          = DC.ar(-0.4261515)
    val mix         = Mix(Seq[GE](
      lFDNoise3_0,
      lFDNoise3_1,
      lFPulse,
      impulse,
      dC,
    ))
    NegatumOut(mix)
  }

  val gIn2: SynthGraph = SynthGraph {
    import de.sciss.synth.GE
    import de.sciss.synth.ugen._
    val inf = Float.PositiveInfinity

    // negatum-77210e85-opt
    NegatumIn()
    val in_0        = Blip.ar(freq = 10.0, numHarm = 1.0)
    val combL       = CombL.ar(in_0, maxDelayTime = 0.0, delayTime = 0.0, decayTime = -0.018617123)
    val min_0       = combL min 0.0
    val phase       = Protect(combL, 0.0, 1.0, false)
    val impulse     = Impulse.ar(freq = 60.0, phase = phase)
    val min_1       = impulse min combL
    val min_2       = min_1 min 0.05332149
    val in_1        = Protect(impulse, -inf, inf, true)
    val protect     = Protect(min_2, 0.0, inf, false)
    val delayTime_0 = protect min 0.05332149
    val delayN      = DelayN.ar(in_1, maxDelayTime = 0.05332149, delayTime = delayTime_0)
    val in_2        = Protect(min_1, -inf, inf, true)
    val coeff       = Protect(min_0, -0.999, 0.999, false)
    val integrator  = Integrator.ar(in_2, coeff = coeff)
    val roundTo     = in_0 roundTo -0.013170808
    val min_3       = roundTo min 0.05332149
    val times       = min_3 * 2.0
    val in_3        = Protect(min_3, -inf, inf, true)
    val allpassN    = AllpassN.ar(in_3, maxDelayTime = 4.4639072, delayTime = 4.4639072,
      decayTime = 3917.4714)
    val dC          = DC.ar(0.48718068)
    val mix         = Mix(Seq[GE](delayN, integrator, times, allpassN, dC))
    NegatumOut(mix)
  }

  // XXX TODO --- what is the purpose of a `Protect` with no boundaries?
  val gOut: SynthGraph = SynthGraph {
    import de.sciss.synth.GE
    import de.sciss.synth.ugen._
    import de.sciss.synth.proc.graph.Ops.stringToControl
    val inf = Float.PositiveInfinity

    NegatumIn()
    val attr_0      = "ctl_6".ar(1.0)
    val freq_0      = Protect(attr_0, -inf, inf, false)
    val lFDNoise3_0 = LFDNoise3.ar(freq_0)
    val attr_1      = "ctl_5".ar(-0.088856705)
    val freq_1      = Protect(attr_1, -inf, inf, false)
    val lFDNoise3_1 = LFDNoise3.ar(freq_1)
    val attr_2      = "ctl_3".ar(0.0)
    val attr_3      = "ctl_4".ar(20000.0)
    val freq_2      = Protect(attr_3, 0.01, 20000.0, false)
    val iphase      = Protect(attr_0, 0.0, 1.0, false)
    val width       = Protect(attr_2, 0.0, 1.0, false)
    val lFPulse     = LFPulse.ar(freq = freq_2, iphase = iphase, width = width)
    val attr_4      = "ctl_1".ar(0.0010397598)
    val attr_5      = "ctl_2".ar(60.0)
    val freq_3      = Protect(attr_5, 0.1, 20000.0, false)
    val phase       = Protect(attr_4, 0.0, 1.0, false)
    val impulse     = Impulse.ar(freq = freq_3, phase = phase)
    val in          = "ctl_0".ar(-0.4261515)
    val dC          = DC.ar(in)
    val mix         = Mix(Seq[GE](lFDNoise3_0, lFDNoise3_1, lFPulse, impulse, dC))
    NegatumOut(mix)

  }
}
