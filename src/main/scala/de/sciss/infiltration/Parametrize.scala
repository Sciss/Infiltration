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

import java.awt.EventQueue

import de.sciss.file._
import de.sciss.fscape.Graph
import de.sciss.fscape.lucre.FScape
import de.sciss.infiltration.SelectionTest.{audioDir, trunkIdMap}
import de.sciss.kollflitz.Vec
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{DoubleObj, IntObj}
import de.sciss.lucre.synth.InMemory
import de.sciss.mellite.{Application, Mellite, Prefs}
import de.sciss.negatum.Negatum.SynthGraphT
import de.sciss.negatum.{Edge, Vertex}
import de.sciss.negatum.impl.{Chromosome, MkSynthGraph, MkTopology, ParamRanges, UGens}
import de.sciss.numbers
import de.sciss.processor.Processor
import de.sciss.span.Span
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.graph.Attribute
import de.sciss.synth.proc.impl.MkSynthGraphSource
import de.sciss.synth.proc.{Bounce, Proc, Runner, TimeRef, Universe}
import de.sciss.synth.ugen.{BinaryOpUGen, RandID}
import de.sciss.synth.{GE, SynthGraph, UGenSpec, audio}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.swing.Swing
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object Parametrize {
  def main(args: Array[String]): Unit = {
    Application.init(Mellite)
    Mellite.initTypes()
    Swing.onEDT(run())
  }

  val VERBOSE = false

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
  
  def mkTestValues(min: Double, max: Double): Vec[Double] = {
    def genPos(n: Int, hi: Double): Vec[Double] = {
      val lo    = math.min(0.05, hi / 10000)
      val n1    = n - 1
      val f     = math.max(1.25, math.pow(hi / lo, 1.0 / n1))
      val vec0  = Vector.tabulate(n)(i => lo * math.pow(f, i))
      vec0.takeWhile(_ + 1.0e-4 < hi) :+ hi
    }

    def genNeg(n: Int, lo: Double): Vec[Double] = {
      val hi    = math.max(-0.05, lo / 10000)
      val n1    = n - 1
      val f     = math.max(1.25, math.pow(lo / hi, 1.0 / n1))
      val vec0  = Vector.tabulate(n)(i => hi * math.pow(f, n1 - i))
      lo +: vec0.dropWhile(_ - 1.0e-4 < lo)
    }

    def genBi(n: Int, lo: Double, hi: Double): Vec[Double] = {
      val n1    = n - 1
      val f     = math.max(1.25, math.pow(hi / lo, 1.0 / n1))
      val vec0  = if (lo > 0.0) Vector.tabulate(n)(i => lo * math.pow(f, i))
                  else          Vector.tabulate(n)(i => hi * math.pow(f, n1 - i))
      lo +: vec0.dropWhile(_ - 1.0e-4 < lo).takeWhile(_ + 1.0e-4 < hi) :+ hi
    }

    if (min == 0.0) {
      assert (max > 0.0)
      0.0 +: genPos(n = 30, hi = max)

    } else if (max == 0.0) {
      assert (min < 0.0)
      genNeg(n = 30, lo = min) :+ 0.0

    } else if (min < 0.0 && max > 0.0) {  // different signum
      genNeg(n = 15, lo = min) ++ (0.0 +: genPos(n = 15, hi = max))

    } else {  // same signum
      assert (math.signum(min) == math.signum(max))
      genBi(n = 31, lo = min, hi = max)
    }
  }
  
//  def include(in: Vec[Double], value: Double): Vec[Double] = {
//    require (in == in.sorted)
//    val idx0 = in.indexWhere(_ > value)
//    val idx  = if (idx0 < 0) in.size else idx0
//    in.patch(idx, value :: Nil, 0)
//  }


  private[this] val inMemory = InMemory()

  // must run on EDT because of preferences
  def bounceVariants(graph: SynthGraph, values: Vec[Double], audioF: File, duration: Double, sampleRate: Int,
                     valueKey: String = "value")
                     (implicit exec: ExecutionContext): Processor[Any] = {
    type I  = InMemory
    implicit val iCursor: I = inMemory

    // val exp = ExprImplicits[I]

    val (objH, _u) = inMemory.step { implicit tx =>
      values.zipWithIndex.map { case (value, gi) =>
        val proc = Proc[I]
        val graphP = graph.copy(
          sources = RandID.ir(gi) +: graph.sources
        )
        proc.graph() = graphP
        proc.attr.put("out"   , IntObj    .newConst(gi))
        proc.attr.put(valueKey, DoubleObj .newConst(value))
        tx.newHandle(proc)
      } -> Universe.dummy[I]
    }
    implicit val u: Universe[I] = _u

    val bncCfg              = Bounce.Config[I]
    bncCfg.group            = objH // :: Nil
    Application.applyAudioPreferences(bncCfg.server, bncCfg.client, useDevice = false, pickPort = false)
    val sCfg                = bncCfg.server
    sCfg.nrtOutputPath      = audioF.path
    sCfg.inputBusChannels   = 0
    sCfg.outputBusChannels  = values.size
    val numPrivate = Prefs.audioNumPrivate.getOrElse(Prefs.defaultAudioNumPrivate)
    import numbers.Implicits._
    sCfg.audioBusChannels   = (sCfg.outputBusChannels + numPrivate).nextPowerOfTwo
    sCfg.wireBuffers        = math.max(sCfg.wireBuffers, 1024) // possibly higher than default
    //    sCfg.blockSize          = 64   // configurable through Mellite preferences now
    sCfg.sampleRate         = sampleRate
    // bc.init : (S#Tx, Server) => Unit
    bncCfg.span             = Span(0L, (duration * TimeRef.SampleRate).toLong)
    val bnc0                = Bounce[I]().apply(bncCfg)
    // tx.afterCommit {
    bnc0.start()
    // }
    bnc0
  }

  final case class Use(vc: Vertex.Constant, min: Double, max: Double)

//  final case class Corr(value: Double, amp: Double)
//  final case class RunOne(testValues: Vec[Double], corr: Vec[Corr])
  final case class RunOne(min: Double, max: Double, corr: Double)

  def futEDT[A](body: => Future[A]): Future[A] =
    if (EventQueue.isDispatchThread) body else {
      val p = Promise[A]()
      Swing.onEDT {
        try {
          p.completeWith(body)
        } catch {
          case NonFatal(ex) =>
            p.tryFailure(ex)
        }
      }
      p.future
    }

  def runOne(topIn: SynthGraphT, use: Use): Future[Option[RunOne]] = {
    val Use(vc, min, max) = use // constWithUse(0) // .head
    val values  = vc.f.toDouble +: mkTestValues(min = min, max = max)
    val vCtl    = ControlVertex("value", Vector(vc.f))
    val topTest = {
      val t0 = topIn.addVertex(vCtl)
      Chromosome.replaceVertex(t0, vOld = vc, vNew = vCtl)
    }
    val graph = MkSynthGraph(topTest)

    val trunkId   = 11
    val tempSpec  = AudioFile.readSpec(audioDir / s"trunk$trunkId/trunk_${trunkIdMap(trunkId)}-1-hilbert-curve.aif")

    import de.sciss.mellite.Mellite.executionContext

//    val bncF = file("/data/temp/_killme.aif")
    val bncF  = File.createTemp(suffix = ".aif")
    val corrF = File.createTemp(suffix = ".aif")
//    println(corrF)

    val futBnc = bounceVariants(graph, values = values, audioF = bncF, duration = tempSpec.numFrames/tempSpec.sampleRate,
      sampleRate = tempSpec.sampleRate.toInt)
    if (VERBOSE) {
      println("Making test bounce...")
    }
    val futCorr = futBnc.flatMap { _ =>
      if (VERBOSE) {
        println("Correlating...")
      }
//      val pReallyDone = Promise[Unit]()

      type I  = InMemory
      implicit val iCursor: I = inMemory

      val r = inMemory.step { implicit tx =>
        val f = FScape[I]()
        f.graph() = gCorr
        val bncLoc  = ArtifactLocation.newConst(bncF  .parent)
        val corrLoc = ArtifactLocation.newConst(corrF .parent)
        f.attr.put("in" , Artifact(bncLoc , bncF  ))
        f.attr.put("out", Artifact(corrLoc, corrF ))
        implicit val u: Universe[I] = Universe.dummy[I]
//        implicit val tgt: ITargets[I] = ITargets.apply
//        import u.workspace
//        implicit val undo: UndoManager[I] = UndoManager.dummy[I]
//        implicit val ctx: Context[I] = Context[I]()
//        val _runMap = ISeq.tabulate(values.size) { ch =>
//          val vr = Var(-1.0)
//          val pair: Ex[(String, Double)] = (s"out-$ch", vr)
//          val _pairI = pair.expand[I]
//          _pairI
//        }
        val _r = f.run(attr =
          Runner.emptyAttr // new IExprAsRunnerMap[I](_runMap, tx)
        )
//        _r.reactNow { implicit tx => state =>
//          if (state.isComplete) {
//            tx.afterCommit {
//              println("Aqui")
//              pReallyDone.success(())
//            }
////            println(s"RESULT: ${_runMap.map(_.value._2)}")
//          }
//        }

        _r
      }
       r.control.status
//      pReallyDone.future
    }
//    Await.result(futCorr, Duration.Inf)
//    println("Done.")
//    while (corrF.length() == 0L) Thread.sleep(100)
    futCorr.map { _  =>
      val afCorr = AudioFile.openRead(corrF)
      try {
        val b = afCorr.buffer(2)
        afCorr.read(b)
        val bt      = b.transpose
        val dCorr   = bt(0).iterator.map(_.toDouble).toVector.tail
        val dEn     = bt(1).iterator.map(_.toDouble).toVector.tail

        //        println(data)
//        RunOne(values, (dCorr zip dEn).map(tup => Corr(tup._1, tup._2)))
        val idxStart = dEn.indexWhere    (_ > 0.01)  // ca. -40 dB
        val idxStop  = dEn.lastIndexWhere(_ > 0.01) + 1
        if (idxStart >= 0 && idxStop > idxStart) {
          val minCorr     = dCorr.slice(idxStart, idxStop).min
          if (minCorr < 0.7) {
            val valueRange  = values.slice(idxStart, idxStop)
            val valueGP     = use.vc.f.toDouble
            val valueMin    = math.min(valueGP, valueRange.head)
            val valueMax    = math.max(valueGP, valueRange.last)
            Some(RunOne(min = valueMin, max = valueMax, corr = minCorr))

          } else {
            None
          }
        } else {
          None
        }

      } finally {
        afCorr.cleanUp()
        corrF.delete()
      }
    }
  }

  def any2stringadd(in: Any): Any = ()

  def sequence[A, B](xs: Seq[A])(f: A => Future[B])(implicit exec: ExecutionContext): Future[Seq[B]] =
    xs.foldLeft(Future.successful(Vector.empty[B])) {
      case (acc, x) =>
        acc.flatMap(prev => f(x).map(prev :+ _))
    }

  lazy val gCorr: Graph = Graph {
    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, _}
    import de.sciss.fscape.lucre.graph._
    //val numFrames = 262144
    val inAll     = AudioFileIn("in")
    val numFrames = inAll.numFrames
    val inRef     = inAll.out(0)
    val inRefRvs  = ReverseWindow(inRef, numFrames)
    val convSize  = numFrames + numFrames - 1
    val fftSize   = convSize.nextPowerOfTwo // numFrames << 1
    //fftSize.poll("fftSize")
    val fftAll    = Real1FFT(inAll    , size = fftSize, mode = 1)
    val fftRef    = Real1FFT(inRefRvs , size = fftSize, mode = 1) * fftSize
    val eAll      = RunningSum(inAll.squared).last
    val eRef      = RunningSum(inRef.squared).last
    val prod      = fftAll.complex * fftRef
    val corr      = Real1IFFT(prod, fftSize, mode = 1)
    //Plot1D(corr, convSize min 1024, "corr")
    val corrMax0  = RunningMax(corr.abs).last
    val corrMax   = corrMax0 / (eAll + eRef) ++ (eAll / eRef)
    AudioFileOut("out", corrMax)
//    MkDouble("out", corrMax)

//    corrMax.ampDb.poll("corrMax [dB]")
    //e1.poll("e1")
    //e2.poll("e2")
  }

  def run(): Unit = {
    val t0 = System.currentTimeMillis()

    val _gIn      = gIn2
    val topIn     = MkTopology(_gIn)
    val numConst  = topIn.vertices.count(_.isConstant)
    val constants = topIn.vertices.collect {
      case vc: Vertex.Constant => vc
    }
    if (VERBOSE) {
      println(s"Num.constants $numConst")
    }

    val constWithUse: Vec[Use] = constants.map { vc =>
      val vNameOut: List[(String, String)] = Chromosome.getArgUsages(topIn, vc).flatMap { edge =>
        edge.sourceVertex match {
          case vu: Vertex.UGen =>
            Some((vu.info.name, edge.inlet))

          case _ => None
        }
      }
      val rOutSeq = vNameOut.map { case (uName, pName) =>
        val r = ParamRanges.map.get(uName)
        val inf = Double.PositiveInfinity
        val (min, max) = r.foldLeft((inf, -inf)) { case ((minA, maxA), info) =>
          val pOpt = info.params.get(pName)
          pOpt.fold((-inf, inf)) { p =>
            (p.lo.fold(-inf)(m => math.min(m.value, minA)), p.hi.fold(inf)(m => math.max(m.value, maxA)))
          }
        }
        (if (min == inf) -inf else min, if (max == -inf) inf else max)
      }
      // always "expand" the possible parameter range, as we'll anyway have a `Protect` in place
      val (minRed, maxRed) = rOutSeq.reduce[(Double, Double)] { case ((minA, maxA), (minB, maxB)) =>
        (math.min(minA, minB), math.max(maxA, maxB))
      }

      // we limit here (unless there is chosen boundary higher than +- 22k)
      val min = if (minRed.isInfinite) -22050.0 else minRed
      val max = if (maxRed.isInfinite) +22050.0 else maxRed

      Use(vc, min, max)
      //      topIn.edgeMap(vc)
    }
//    val constWithUse = constWithUse0.filterNot { case (_, uses) =>
//      uses.isEmpty || uses.forall(u => u._2.exists(_.dynamic))
//    }

    if (VERBOSE) {
      println(constants.map(_.f))
      println(constWithUse.mkString("\n"))
    }

    /*

     take the current value. roundUpTo(0.1). if this is zero, add 0.1; go in up to
     16 octaves up (times 2, times 2, ...); or stop when reaching the worst upper
     param range; do the same in the opposite direction; if

     or more coarse; factor 3

     these 31 values:

     (0 until 15).map(i => 22050 / 2.5.pow(i)) ++ 0.0 ++ (0 until 15).map(i => -22050 / 2.5.pow(i))

     should suffice to detect if the parameter has sonic effect, and the approximate ranges

     if a param range is present, we could lower the factor 2.5 for more fine-grained boundaries

     we can bounce in "parallel" channels if we use multiple synths each of which has a unique RNG identifier,
     although perhaps this is not any faster than bouncing a "timeline" of 31 successive runs

     for the correlation purposes, we should insert the original value, so we have actually 32 runs

    */

    import de.sciss.mellite.Mellite.executionContext

//    val use     = constWithUse(10)
//    val futOne  = runOne(topIn, use)
//    futOne.foreach { opt =>
//      println(opt)
//    }

    val futCorr = sequence(constWithUse) { use =>
      futEDT {
        runOne(topIn, use)
      }
    }

    futCorr.onComplete { tr =>
      val t1 = System.currentTimeMillis()
      println(s"Done (took ${(t1 - t0)/1000}s). Ok? ${tr.isSuccess}")
      tr match {
        case Success(seq) =>
          if (VERBOSE) {
            println(seq.mkString("\n"))
          }
          val hasRun = (seq zip constWithUse).collect {
            case (Some(run), vc) => (run, vc)
          }
          val sortRun = hasRun.sortBy(_._1.corr).take(5)
          println("\n--SEL---\n")
          println(sortRun.mkString("\n"))

          val topPatch = sortRun.zipWithIndex.foldLeft(topIn) { case (topAcc, ((run, use), idx)) =>
            import use.vc
            import numbers.Implicits._
            val v0      = vc.f.linLin(run.min, run.max, 0.0, 1.0)
            val vCtl    = ControlVertex(s"ctl_$idx", Vector(v0.toFloat))
            val nMul    = s"Bin_${BinaryOpUGen.Times.id}"
            val nAdd    = s"Bin_${BinaryOpUGen.Plus .id}"
            val sMul    = UGens.map(nMul)
            val sAdd    = UGens.map(nAdd)
            val vMul    = Vertex.UGen(sMul)
            val vAdd    = Vertex.UGen(sAdd)
            val vcMul   = Vertex.Constant((run.max - run.min).toFloat)
            val vcAdd   = Vertex.Constant( run.min.toFloat)
            var topOut  = topAcc
            topOut      = topOut.addVertex(vCtl)
            topOut      = topOut.addVertex(vMul)
            topOut      = topOut.addVertex(vAdd)
            topOut      = topOut.addEdge(Edge(vMul, vCtl  , "a")).get._1
            topOut      = topOut.addVertex(vcMul)
            topOut      = topOut.addEdge(Edge(vMul, vcMul , "b")).get._1
            topOut      = topOut.addEdge(Edge(vAdd, vMul  , "a")).get._1
            topOut      = topOut.addVertex(vcAdd)
            topOut      = topOut.addEdge(Edge(vAdd, vcAdd , "b")).get._1
            topOut      = Chromosome.replaceVertex(topOut, vOld = vc, vNew = vAdd)
            topOut
          }

          val gPatch    = MkSynthGraph(topPatch)
          val srcPatch  = MkSynthGraphSource(gPatch)
          println()
          println(srcPatch)

        case Failure(ex) =>
          println(ex)
      }
    }


//      println(data.map { corr =>
//        import numbers.Implicits._
//        (corr.value.ampDb, corr.amp.ampDb)
//      })
//      val idxStart = data.indexWhere    (_.amp > 0.01)  // ca. -40 dB
//      val idxStop  = data.lastIndexWhere(_.amp > 0.01) + 1
//      if (idxStart >= 0 && idxStop > idxStart) {
//        val minCorr     = data.slice(idxStart, idxStop).map(_.value).min
//        val valueRange  = testValues.slice(idxStart, idxStop)
//        val valueGP     = use.vc.f.toDouble
//        val valueMin    = math.min(valueGP, valueRange.head)
//        val valueMax    = math.max(valueGP, valueRange.last)
//        import numbers.Implicits._
//        println(f"VALUES (gp was ${use.vc.f}): $valueMin to $valueMax ; minCorr = ${minCorr.ampDb}%g dB")
//      } else {
//        println("Nope.")
//      }

//    val topOut = constants.zipWithIndex.foldLeft(topIn) { case (topAcc, (vc, idx)) =>
//      val vCtl = ControlVertex(s"ctl_$idx", Vector(vc.f))
//      val topAdd = topAcc.addVertex(vCtl)
//      Chromosome.replaceVertex(topAdd, vOld = vc, vNew = vCtl)
//    }
//
//    val _gOut   = MkSynthGraph(topOut)
//    val srcOut  = MkSynthGraphSource(_gOut)
//    println(srcOut)
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
    import de.sciss.synth.proc.graph.Ops.stringToControl
    import de.sciss.synth.ugen._
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
