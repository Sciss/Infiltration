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
import java.util.{Timer, TimerTask}

import de.sciss.file._
import de.sciss.fscape.Graph
import de.sciss.fscape.lucre.FScape
import de.sciss.infiltration.Implicits.ProcessorOps
import de.sciss.infiltration.OptimizeWorkspace.ProcSpec
import de.sciss.kollflitz.Vec
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{DoubleObj, DoubleVector, IntObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Folder
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.{InMemory, Sys}
import de.sciss.mellite.{Application, Mellite, Prefs}
import de.sciss.negatum.Negatum.SynthGraphT
import de.sciss.negatum.Vertex
import de.sciss.negatum.impl.{Chromosome, MkSynthGraph, MkTopology, ParamRanges}
import de.sciss.nuages.{ExponentialWarp, LinearWarp, ParamSpec}
import de.sciss.numbers
import de.sciss.span.Span
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.graph.{Attribute, Param}
import de.sciss.synth.proc.{Bounce, Durable, Proc, Runner, TimeRef, Universe, Workspace}
import de.sciss.synth.ugen.{NegatumOut, RandID}
import de.sciss.synth.{GE, SynthGraph, UGenSpec, audio}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.swing.Swing
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object Parametrize {
  def init(): Unit = {
    Application.init(Mellite)
    Mellite.initTypes()
    RunNegatum.tweak()
  }

  def main(args: Array[String]): Unit = {
    if (args.headOption.contains("--test")) {
      test()
      return
    }

    require (args.length == 1, "Must provide a workspace .mllt argument")
    val wsDir = file(args(0))

    Parametrize.init()
    NegatumOut.MONO = false

    type S  = Durable
    val dsf = BerkeleyDB.factory(wsDir, createIfNecessary = false)
    val _ws = Workspace.Durable.read(wsDir, dsf)
    val (iterMap: Map[Int, Vec[ProcSpec]], folderOutH) = _ws.system.step { implicit tx =>
      val r         = _ws.root
      val folderIn  = r.$[Folder]("opt").getOrElse(sys.error("No folder 'opt' found"))
      val folderOut = r.$[Folder]("par").getOrElse {
        val f = Folder[S]()
        f.name = "par"
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

    implicit val timer: Timer = new Timer
    val numExisting = cursor.step { implicit tx => folderOutH().size }
    println(s"Iterations: $numExisting of ${iterKeys0.size}")

    val iterKeys = iterKeys0.drop(numExisting)
    val futAll = Parametrize.sequenceUnit(iterKeys) { iterIdx =>
      println(s"Iteration $iterIdx")
      val folderItH = cursor.step { implicit tx =>
        val folderOut = folderOutH()
        val folderIt  = Folder[S]()
        folderIt.name = s"It $iterIdx"
        folderOut.addLast(folderIt)
        tx.newHandle(folderIt)
      }
      val procs = iterMap(iterIdx)
      Parametrize.sequenceUnit(procs) { procSpec =>
        println(s"  ${procSpec.name}")

        val o = runGraph(procSpec.graph)
        o.transform { tr =>
          tr match {
            case Success(res) =>
              cursor.step { implicit tx =>
                val folderIt  = folderItH()
                val pOut      = Proc[S]()
                pOut.name     = s"${procSpec.name}-par"
                pOut.graph()  = res.graph
                val pAttr     = pOut.attr
//                pAttr.put(Proc.attrSource, Code.Obj.newVar(Code.Obj.newConst(
//                  Code.SynthGraph(res.source))))
                res.specs.zipWithIndex.foreach { case ((default, spec), si) =>
                  val defaultN  = Vector.fill(4)(default)
                  val paramObj  = DoubleVector.newVar(DoubleVector.newConst[S](defaultN))
                  val specObj   = ParamSpec.Obj.newConst[S](spec)
                  val key       = s"p${si + 1}"
                  val specKey   = ParamSpec.composeKey(key)
                  pAttr.put(specKey , specObj )
                  pAttr.put(key     , paramObj)
                }
                folderIt.addLast(pOut)
              }

            case Failure(ex) =>
              println("... failed : ")
              println(ex)
              ex.printStackTrace()
          }

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

  def test(): Unit = {
    init()
    Swing.onEDT {
      implicit val timer: Timer = new Timer
      val fut = runGraph(gIn10)
      import de.sciss.mellite.Mellite.executionContext
      fut.onComplete { tr =>
        if (tr.isFailure) println(tr)
        sys.exit(if (tr.isSuccess) 0 else 1)
      }
    }
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

    def copy(): Vertex = ControlVertex(name = name, values = values)
  }

  final case class ParamVertex(name: String, /*spec: ParamSpec,*/ default: Vec[Float]) extends Vertex.UGen {
    val info: UGenSpec = UGenSpec(
      name        = "ParamVertex",
      attr        = Set.empty,
      rates       = UGenSpec.Rates.Implied(audio, UGenSpec.RateMethod.Default),
      args        = Vector.empty,
      inputs      = Vector.empty,
      outputs     = Vector.tabulate(default.size) { _ =>
        UGenSpec.Output(name = None, shape = UGenSpec.SignalShape.Generic, variadic = None)
      },
      doc         = None,
      elemOption  = None,
    )

    def instantiate(ins: Vec[(AnyRef, Class[_])]): GE =
      Param.ar(name, default)

    def copy(): Vertex = ParamVertex(name = name, /*spec = spec,*/ default = default)
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
                     (implicit exec: ExecutionContext, timer: Timer): Future[Any] = {
    type I  = InMemory
    implicit val iCursor: I = inMemory

    // val exp = ExprImplicits[I]

    val (objH, _u) = inMemory.step { implicit tx =>
      values.zipWithIndex.map { case (value, gi) =>
        val proc = Proc[I]()
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

    val bncCfg              = Bounce.Config[I]()
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
//    bnc0
    bnc0.startWithTimeout(10.0)
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

//  private var currentBnc = Option.empty[Processor[Any]]  // XXX TODO hackish

  def runOne(topIn: SynthGraphT, use: Use, dur: Double, sampleRate: Double)
            (implicit timer: Timer): Future[Option[RunOne]] = {
    val Use(vc, min, max) = use // constWithUse(0) // .head
    val testValues  = mkTestValues(min = min, max = max)
    val default     = vc.f.toDouble
    val values      = default +: testValues
    val vCtl        = ControlVertex("value", Vector(vc.f))
    val topTest     = {
      val t0 = topIn.addVertex(vCtl)
      Chromosome.replaceVertex(t0, vOld = vc, vNew = vCtl)
    }
    val graph = MkSynthGraph(topTest)

    import de.sciss.mellite.Mellite.executionContext

//    val bncF = file("/data/temp/_killme.aif")
    val bncF  = File.createTemp(suffix = ".aif")
    val corrF = File.createTemp(suffix = ".aif")
//    println(corrF)

    val futBnc = bounceVariants(
      graph       = graph,
      values      = values,
      audioF      = bncF,
      duration    = dur, // tempSpec.numFrames/tempSpec.sampleRate
      sampleRate  = sampleRate.toInt,
    )
//    currentBnc = Some(futBnc)

    if (VERBOSE) {
      println("Making test bounce...")
    }
    val futCorr0 = futBnc.flatMap { _ =>
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

      val ttTimeOut = new TimerTask {
        def run(): Unit = {
          println("FScape timeout")
          inMemory.step { implicit tx => r.cancel() }
        }
      }
      timer.schedule(ttTimeOut, 15000L)

      val futFSc0 = r.control.status

      val futFSc = futFSc0.transform { tr =>
        ttTimeOut.cancel()
        tr
      }

      futFSc
    }

    val futCorr = futCorr0.transform { tr =>
      bncF.delete()
      tr
    }

//    Await.result(futCorr, Duration.Inf)
//    println("Done.")
//    while (corrF.length() == 0L) Thread.sleep(100)
    val futOut0 = futCorr.map { _  =>
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
            val valueRange  = testValues.slice(idxStart, idxStop)
            val valueGP     = use.vc.f.toDouble
            val valueMin    = math.min(valueGP, valueRange.head)
            val valueMax    = math.max(valueGP, valueRange.last)
            if (valueMin == valueMax) {
              println("Häh???? " + valueRange + "; " + valueGP.toString)
            } else if (valueMin.isNaN || valueMax.isNaN) {
              println("min " + valueMin.toString + "; max " + valueMax.toString)
            }
            Some(RunOne(min = valueMin, max = valueMax, corr = minCorr))

          } else {
            None
          }
        } else {
          None
        }

      } finally {
        afCorr.cleanUp()
      }
    }

    val futOut = futOut0.transform { tr =>
      corrF.delete()
      tr
    }

    futOut

  }

  def sequenceUnit[A](xs: Seq[A])(f: A => Future[Any])(implicit exec: ExecutionContext): Future[Unit] =
    xs.foldLeft(Future.successful(())) {
      case (acc, x) =>
        acc.flatMap(_ => f(x).map(_ => ()))
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

  case class Result(graph: SynthGraph, specs: Seq[(Double, ParamSpec)] /*, source: String*/)

  def runGraph(_gIn: SynthGraph, dur: Double = 5.9, sampleRate: Double = 44100.0)
              (implicit timer: Timer): Future[Result] = {
    val t0 = System.currentTimeMillis()

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
        runOne(topIn, use, dur = dur, sampleRate = sampleRate)
      }
    }

    futCorr.map { seq =>
      val t1 = System.currentTimeMillis()
      println(s"  Done (took ${(t1 - t0)/1000}s).")

      if (VERBOSE) {
        println(seq.mkString("\n"))
      }
      val hasRun: Seq[(RunOne, Use)] = (seq zip constWithUse).collect {
        case (Some(run), vc) => (run, vc)
      }
      val sortRun = hasRun.sortBy(_._1.corr).take(5)

      if (VERBOSE) {
        println("\n--SEL---\n")
        println(sortRun.mkString("\n"))
      } else {
        println(s"  num-params: ${sortRun.size}")
      }

//          val topPatch = sortRun.zipWithIndex.foldLeft(topIn) { case (topAcc, ((run, use), idx)) =>
//            import use.vc
//            import numbers.Implicits._
//            val v0      = vc.f.linLin(run.min, run.max, 0.0, 1.0)
//            val vCtl    = ControlVertex(s"ctl_$idx", Vector(v0.toFloat))
//            val nMul    = s"Bin_${BinaryOpUGen.Times.id}"
//            val nAdd    = s"Bin_${BinaryOpUGen.Plus .id}"
//            val sMul    = UGens.map(nMul)
//            val sAdd    = UGens.map(nAdd)
//            val vMul    = Vertex.UGen(sMul)
//            val vAdd    = Vertex.UGen(sAdd)
//            val vcMul   = Vertex.Constant((run.max - run.min).toFloat)
//            val vcAdd   = Vertex.Constant( run.min.toFloat)
//            var topOut  = topAcc
//            topOut      = topOut.addVertex(vCtl)
//            topOut      = topOut.addVertex(vMul)
//            topOut      = topOut.addVertex(vAdd)
//            topOut      = topOut.addEdge(Edge(vMul, vCtl  , "a")).get._1
//            topOut      = topOut.addVertex(vcMul)
//            topOut      = topOut.addEdge(Edge(vMul, vcMul , "b")).get._1
//            topOut      = topOut.addEdge(Edge(vAdd, vMul  , "a")).get._1
//            topOut      = topOut.addVertex(vcAdd)
//            topOut      = topOut.addEdge(Edge(vAdd, vcAdd , "b")).get._1
//            topOut      = Chromosome.replaceVertex(topOut, vOld = vc, vNew = vAdd)
//            topOut
//          }

      val topPatch = sortRun.zipWithIndex.foldLeft(topIn) { case (topAcc, ((_, use), idx)) =>
        import use.vc
        val vCtl    = ParamVertex(s"p${idx + 1}", /*spec,*/ Vector.fill(4)(vc.f))
        var topOut  = topAcc
        topOut      = topOut.addVertex(vCtl)
        topOut      = Chromosome.replaceVertex(topOut, vOld = vc, vNew = vCtl)
        topOut
      }

      val specs     = sortRun.map { case (run, use) =>
        import use.vc
        val warp    = if (run.min.sign == run.max.sign) ExponentialWarp else LinearWarp
        val spec    = ParamSpec(run.min, run.max, warp)
        val default = spec.inverseMap(vc.f)
        (default, spec)
      }

      val gPatch    = MkSynthGraph(topPatch, expandProtect = true, expandIO = true)
//      val srcPatch  = MkSynthGraphSource(gPatch)

//      println()
//      println(srcPatch)

      Result(gPatch, specs = specs /*, srcPatch*/)
    }
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
  lazy val gIn1: SynthGraph = SynthGraph {
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

  lazy val gIn2: SynthGraph = SynthGraph {
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

  lazy val gIn3: SynthGraph = SynthGraph {
    import de.sciss.synth.GE
    import de.sciss.synth.ugen._
    val inf = Float.PositiveInfinity

    // negatum-d14aeb62-opt

    NegatumIn()
    val blip          = Blip.ar(freq = 10.0, numHarm = 1.0)
    val min_0         = (0.010628436: GE) min blip
    val lFPulse_0     = LFPulse.ar(freq = 3253.167, iphase = 0.0, width = 1.0)
    val minus         = lFPulse_0 - -2.6010132
    val freq_0        = Protect(lFPulse_0, -inf, inf, false)
    val lFDClipNoise  = LFDClipNoise.ar(freq_0)
    val in_0          = Protect(min_0, -inf, inf, true)
    val twoPole       = TwoPole.ar(in_0, freq = 10.0, radius = 0.0070142653)
    val impulse       = Impulse.ar(freq = 0.49309492, phase = 0.0)
    val in_1          = Protect(impulse, -inf, inf, true)
    val revTime       = Protect(minus, 0.0, 100.0, false)
    val gVerb         = GVerb.ar(in_1, roomSize = 0.95, revTime = revTime, damping = 0.0070142653,
      inputBW = 0.98530567, spread = 1.7910538, dryLevel = 0.0,
      earlyRefLevel = 0.18113671, tailLevel = 5.238983, maxRoomSize = 0.95)
    val lFPulse_1     = LFPulse.ar(freq = 20000.0, iphase = 0.0, width = 0.0067190463)
    val lFDNoise1     = LFDNoise1.ar(0.49309492)
    val min_1         = (0.0: GE) min twoPole
    val in_2          = Protect(min_1, -inf, inf, true)
    val rq            = Protect(0.01, 0.01, 100.0, false)
    val bPF           = BPF.ar(in_2, freq = 10.0, rq = rq)
    val dC            = DC.ar(-0.1571754)
    val mix           = Mix(Seq[GE](lFDClipNoise, gVerb, lFPulse_1, lFDNoise1, bPF, dC))
    NegatumOut(mix)
  }

  lazy val gIn4: SynthGraph = SynthGraph {
    import de.sciss.synth.GE
    import de.sciss.synth.ugen._
    val inf = Float.PositiveInfinity

    // negatum-c999800-opt

    NegatumIn()
    val impulse     = Impulse.ar(freq = 3488.15, phase = 0.10654729)
    val in_0        = Protect(impulse, -inf, inf, true)
    val hPZ1        = HPZ1.ar(in_0)
    val in_1        = Protect(hPZ1, -inf, inf, true)
    val integrator  = Integrator.ar(in_1, coeff = 0.0068862666)
    val min_0       = integrator min 0.38350186
    val min_1       = min_0 min 0.0
    val in_2        = Protect(hPZ1, -inf, inf, true)
    val hPF         = HPF.ar(in_2, freq = 10.0)
    val in_3        = Protect(min_0, -inf, inf, true)
    val freqShift   = FreqShift.ar(in_3, freq = 0.6309788, phase = -89.7749)
    val mix         = Mix(Seq[GE](min_1, hPF, freqShift))
    NegatumOut(mix)
  }

  lazy val gIn5: SynthGraph = SynthGraph {
    import de.sciss.synth.GE
    import de.sciss.synth.ugen._
    val inf = Float.PositiveInfinity

    // negatum-e4bd0161-opt

    NegatumIn()
    val lFDClipNoise  = LFDClipNoise.ar(1592712.5)
    val decayTime     = (0.0: GE) min lFDClipNoise
    val times_0       = decayTime * 5.0
    val minus         = (0.0: GE) - decayTime
    val in_0          = Protect(decayTime, -inf, inf, true)
    val hPZ1          = HPZ1.ar(in_0)
    val min_0         = decayTime min hPZ1
    val times_1       = min_0 * 3.0
    val absdif        = min_0 absDif lFDClipNoise
    val in_1          = Protect(absdif, -inf, inf, true)
    val combC         = CombC.ar(in_1, maxDelayTime = 0.0, delayTime = 0.0, decayTime = decayTime)
    val in_2          = Protect(decayTime, -inf, inf, true)
    val attack        = Protect(1.0E-6, 1.0E-6, 30.0, false)
    val release       = Protect(combC, 1.0E-6, 30.0, false)
    val decay2        = Decay2.ar(in_2, attack = attack, release = release)
    val in_3          = Protect(decayTime, -inf, inf, true)
    val hPZ2          = HPZ2.ar(in_3)
    val in_4          = Protect(hPZ2, -inf, inf, true)
    val delayC        = DelayC.ar(in_4, maxDelayTime = 0.0, delayTime = 0.0)
    val min_1         = hPZ2 min minus
    val in_5          = Protect(min_0, -inf, inf, true)
    val freq_0        = Protect(min_0, -20000.0, 20000.0, false)
    val phase         = Protect(decayTime, -inf, inf, false)
    val in_6          = FreqShift.ar(in_5, freq = freq_0, phase = phase)
    val leakDC        = LeakDC.ar(in_6, coeff = 0.8)
    val in_7          = Protect(in_6, -inf, inf, true)
    val pitchShift    = PitchShift.ar(in_7, winSize = 0.001, pitchRatio = 0.0, pitchDispersion = 1.0,
      timeDispersion = 0.0)
    val min_2         = pitchShift min delayC
    val in_8          = Protect(min_2, -inf, inf, true)
    val dur           = Protect(1.0E-6, 1.0E-6, 30.0, false)
    val ramp          = Ramp.ar(in_8, dur = dur)
    val mix           = Mix(Seq[GE](times_0, times_1, decay2, min_1, leakDC, ramp))
    NegatumOut(mix)
  }

  lazy val gIn6: SynthGraph = SynthGraph {
    import de.sciss.synth.GE
    import de.sciss.synth.ugen._
    val inf = Float.PositiveInfinity
    // negatum-92d59eaa-opt
    NegatumIn()
    val lFDClipNoise  = LFDClipNoise.ar(46551.492)
    val min_0         = lFDClipNoise min 0.00605236
    val min_1         = (46551.492: GE) min lFDClipNoise
    val in_0          = Protect(min_1, -inf, inf, true)
    val bPZ2          = BPZ2.ar(in_0)
    val in_1          = Protect(min_1, -inf, inf, true)
    val ramp_0        = Ramp.ar(in_1, dur = 30.0)
    val min_2         = ramp_0 min min_1
    val min_3         = min_2 min 0.00605236
    val in_2          = Protect(min_3, -inf, inf, true)
    val dur_0         = Protect(min_3, 1.0E-6, 30.0, false)
    val ramp_1        = Ramp.ar(in_2, dur = dur_0)
    val phase_0       = Protect(min_1, 0.0, 1.0, false)
    val impulse       = Impulse.ar(freq = 0.1, phase = phase_0)
    val in_3          = Protect(impulse, -inf, inf, true)
    val delay1        = Delay1.ar(in_3)
    val min_4         = impulse min ramp_1
    val min_5         = (0.00605236: GE) min min_4
    val min_6         = ramp_0 min ramp_1
    val min_7         = (0.0: GE) min min_5
    val min_8         = min_6 min min_2
    val hypot         = min_8 hypot min_8
    val in_4          = Protect(min_5, -inf, inf, true)
    val freq_0        = Protect(ramp_0, -20000.0, 20000.0, false)
    val phase_1       = Protect(hypot, -inf, inf, false)
    val freqShift     = FreqShift.ar(in_4, freq = freq_0, phase = phase_1)
    val wrap2         = min_5 wrap2 min_8
    val ring2_0       = min_6 ring2 wrap2
    val min_9         = ring2_0 min min_7
    val in_5          = Protect(hypot, -inf, inf, true)
    val attack        = Protect(wrap2, 1.0E-6, 30.0, false)
    val release       = Protect(ring2_0, 1.0E-6, 30.0, false)
    val decay2        = Decay2.ar(in_5, attack = attack, release = release)
    val min_10        = ring2_0 min decay2
    val min_11        = min_8 min min_10
    val protect       = Protect(min_11, -20.0, 20.0, false)
    val sinh          = protect.sinh
    val times         = sinh * 2.0
    val in_6          = Protect(min_6, -inf, inf, true)
    val dur_1         = Protect(bPZ2, 1.0E-6, 30.0, false)
    val ramp_2        = Ramp.ar(in_6, dur = dur_1)
    val min_12        = ramp_2 min freqShift
    val min_13        = hypot min ramp_2
    val min_14        = min_13 min min_3
    val min_15        = sinh min min_0
    val in_7          = Protect(sinh, -inf, inf, true)
    val bRZ2          = BRZ2.ar(in_7)
    val min_16        = sinh min min_11
    val ring2_1       = ramp_1 ring2 min_5
    val min_17        = sinh min min_14
    val mix           = Mix(Seq[GE](delay1, min_9, times, min_12, min_15, bRZ2, min_16, ring2_1, min_17))
    NegatumOut(mix)
  }

  lazy val gIn7: SynthGraph = SynthGraph {
    import de.sciss.synth.GE
    import de.sciss.synth.ugen._
    val inf = Float.PositiveInfinity
    // negatum-60d6a6830-opt
    NegatumIn()
    val coeff         = LFDClipNoise.ar(46551.492)
    val sinh          = coeff.sinh
    val freq_0        = Protect(sinh, 0.1, 20000.0, false)
    val phase         = Protect(sinh, 0.0, 1.0, false)
    val impulse       = Impulse.ar(freq = freq_0, phase = phase)
    val min_0         = (0.003049939: GE) min impulse
    val difsqr        = min_0 difSqr 0.0
    val min_1         = coeff min 0.0
    val min_2         = coeff min min_0
    val in_0          = Protect(min_2, -inf, inf, true)
    val oneZero       = OneZero.ar(in_0, coeff = coeff)
    val min_3         = oneZero min 0.0
    val min_4         = coeff min min_3
    val blip          = Blip.ar(freq = 10.0, numHarm = 1.0)
    val excess        = (0.0: GE) excess blip
    val min_5         = (0.0: GE) min blip
    val min_6         = min_4 min min_5
    val freq_1        = excess > min_3
    val lFDNoise1     = LFDNoise1.ar(freq_1)
    val min_7         = lFDNoise1 min 0.0
    val ring2         = min_5 ring2 min_7
    val min_8         = ring2 min min_6
    val min_9         = min_8 min coeff
    val in_1          = Protect(min_6, -inf, inf, true)
    val rq            = Protect(0.01, 0.01, 100.0, false)
    val rLPF          = RLPF.ar(in_1, freq = 10.0, rq = rq)
    val lFDClipNoise  = LFDClipNoise.ar(46551.492)
    val dC            = DC.ar(1.0E-6)
    val mix           = Mix(Seq[GE](difsqr, min_1, min_9, rLPF, lFDClipNoise, dC))
    NegatumOut(mix)
  }

  lazy val gIn8: SynthGraph = SynthGraph {
    import de.sciss.synth.GE
    import de.sciss.synth.ugen._
    val inf = Float.PositiveInfinity
    // negatum-90b3678f-opt
    NegatumIn()
    val freq_0    = Protect(287469.03, 0.01, 20000.0, false)
    val lFPulse   = LFPulse.ar(freq = freq_0, iphase = 0.00605236, width = 1.0)
    val in_0      = Protect(lFPulse, -inf, inf, true)
    val hPZ2_0    = HPZ2.ar(in_0)
    val min_0     = (0.00605236: GE) min hPZ2_0
    val cos       = min_0.cos
    val phase     = Protect(min_0, 0.0, 1.0, false)
    val impulse   = Impulse.ar(freq = 0.1, phase = phase)
    val in_1      = Protect(hPZ2_0, -inf, inf, true)
    val hPZ2_1    = HPZ2.ar(in_1)
    val min_1     = hPZ2_1 min impulse
    val in_2      = Protect(min_0, -inf, inf, true)
    val lPZ1      = LPZ1.ar(in_2)
    val gt        = lPZ1 > min_1
    val in_3      = min_0 min 0.0
    val decayTime = in_3 min lPZ1
    val times     = decayTime * 2.0
    val in_4      = Protect(impulse, -inf, inf, true)
    val time_0    = Protect(impulse, 1.0E-6, 30.0, false)
    val lag       = Lag.ar(in_4, time = time_0)
    val in_5      = Protect(in_3, -inf, inf, true)
    val time_1    = Protect(lag, 1.0E-6, 30.0, false)
    val lag2_0    = Lag2.ar(in_5, time = time_1)
    val in_6      = Protect(impulse, -inf, inf, true)
    val time_2    = Protect(gt, 1.0E-6, 30.0, false)
    val lag2_1    = Lag2.ar(in_6, time = time_2)
    val in_7      = Protect(min_0, -inf, inf, true)
    val freq_1    = Protect(decayTime, -20000.0, 20000.0, false)
    val in_8      = FreqShift.ar(in_7, freq = freq_1, phase = 0.0)
    val min_2     = decayTime min lag
    val min_3     = in_8 min min_2
    val in_9      = Protect(lag2_1, -inf, inf, true)
    val allpassL  = AllpassL.ar(in_9, maxDelayTime = 0.0, delayTime = 0.0, decayTime = decayTime)
    val leakDC_0  = LeakDC.ar(in_8, coeff = 0.8)
    val min_4     = min_3 min leakDC_0
    val leakDC_1  = LeakDC.ar(in_3, coeff = 0.8)
    val hypot     = min_2 hypot 0.0
    val mix       = Mix(Seq[GE](cos, times, lag2_0, allpassL, min_4, leakDC_1, hypot))
    NegatumOut(mix)
  }

  lazy val gIn9: SynthGraph = SynthGraph {
    import de.sciss.synth.GE
    import de.sciss.synth.ugen._
    val inf = Float.PositiveInfinity
    // negatum-f8f750c2-opt
    NegatumIn()
    val decayTime_0     = Impulse.ar(freq = 60.0, phase = 1.0)
    val in_0            = Protect(decayTime_0, -inf, inf, true)
    val lPZ1            = LPZ1.ar(in_0)
    val atan2           = (36.82512: GE) atan2 decayTime_0
    val in_1            = Protect(decayTime_0, -inf, inf, true)
    val dur_0           = Protect(decayTime_0, 1.0E-6, 30.0, false)
    val ramp_0          = Ramp.ar(in_1, dur = dur_0)
    val in_2            = Protect(atan2, -inf, inf, true)
    val bPZ2_0          = BPZ2.ar(in_2)
    val min_0           = bPZ2_0 min decayTime_0
    val min_1           = lPZ1 min ramp_0
    val min_2           = min_1 min 0.17019051
    val min_3           = min_2 min min_0
    val decayTime_1     = decayTime_0.cos
    val in_3            = Protect(atan2, -inf, inf, true)
    val dur_1           = Protect(bPZ2_0, 1.0E-6, 30.0, false)
    val ramp_1          = Ramp.ar(in_3, dur = dur_1)
    val min_4           = ramp_1 min min_3
    val min_5           = ramp_1 min 0.0
    val blip            = Blip.ar(freq = 10.0, numHarm = 1.0)
    val min_6           = blip min min_3
    val min_7           = min_4 min min_6
    val min_8           = min_5 min lPZ1
    val times           = min_6 * min_7
    val in_4            = Protect(ramp_1, -inf, inf, true)
    val maxDelayTime_0  = Protect(times, 0.0, 20.0, false)
    val delayTime_0     = (0.0: GE) min maxDelayTime_0
    val allpassL        = AllpassL.ar(in_4, maxDelayTime = maxDelayTime_0, delayTime = delayTime_0,
      decayTime = decayTime_0)
    val in_5            = Protect(min_7, -inf, inf, true)
    val hPZ1            = HPZ1.ar(in_5)
    val min_9           = min_3 min allpassL
    val min_10          = min_9 min hPZ1
    val sqrdif          = min_10 sqrDif ramp_1
    val min_11          = min_4 min min_9
    val eq              = min_7 sig_== min_11
    val in_6            = Protect(eq, -inf, inf, true)
    val rq_0            = Protect(0.01, 0.01, 100.0, false)
    val rLPF            = RLPF.ar(in_6, freq = 10.0, rq = rq_0)
    val min_12          = min_3 min rLPF
    val in_7            = Protect(min_0, -inf, inf, true)
    val maxDelayTime_1  = Protect(rLPF, 0.0, 20.0, false)
    val delayTime_1     = (0.0: GE) min maxDelayTime_1
    val combL           = CombL.ar(in_7, maxDelayTime = maxDelayTime_1, delayTime = delayTime_1,
      decayTime = decayTime_1)
    val min_13          = blip min combL
    val in_8            = Protect(min_13, -inf, inf, true)
    val time            = Protect(sqrdif, 1.0E-6, 30.0, false)
    val lag             = Lag.ar(in_8, time = time)
    val in_9            = Protect(times, -inf, inf, true)
    val freq_0          = Protect(allpassL, -20000.0, 20000.0, false)
    val phase_0         = Protect(lag, -inf, inf, false)
    val freqShift_0     = FreqShift.ar(in_9, freq = freq_0, phase = phase_0)
    val min_14          = freqShift_0 min ramp_1
    val in_10           = Protect(min_4, -inf, inf, true)
    val bPZ2_1          = BPZ2.ar(in_10)
    val min_15          = sqrdif min bPZ2_1
    val min_16          = min_15 min min_9
    val min_17          = min_15 min min_3
    val in_11           = Protect(min_13, -inf, inf, true)
    val coeff           = Protect(min_4, -1.0, 1.0, false)
    val oneZero         = OneZero.ar(in_11, coeff = coeff)
    val min_18          = oneZero min min_7
    val in_12           = Protect(min_18, -inf, inf, true)
    val freq_1          = Protect(min_8, -20000.0, 20000.0, false)
    val phase_1         = Protect(lag, -inf, inf, false)
    val freqShift_1     = FreqShift.ar(in_12, freq = freq_1, phase = phase_1)
    val in_13           = Protect(min_8, -inf, inf, true)
    val rq_1            = Protect(freqShift_1, 0.01, 100.0, false)
    val resonz          = Resonz.ar(in_13, freq = 10.0, rq = rq_1)
    val min_19          = min_8 min resonz
    val min_20          = blip min min_12
    val min_21          = rLPF min 0.0
    val neq             = min_19 sig_!= min_3
    val phase_2         = Protect(bPZ2_1, 0.0, 1.0, false)
    val impulse         = Impulse.ar(freq = 0.1, phase = phase_2)
    val gt              = (0.0: GE) > min_7
    val min_22          = min_8 min freqShift_1
    val geq             = min_4 >= min_12
    val mix             = Mix(
      Seq[GE](min_14, min_16, min_17, min_20, min_21, neq, impulse, gt, min_22, geq))
    NegatumOut(mix)
  }

  lazy val gIn10: SynthGraph = SynthGraph {
    import de.sciss.synth.GE
    import de.sciss.synth.ugen._
    val inf = Float.PositiveInfinity
    // negatum-864a4fe-opt
    NegatumIn()
    val freq          = Protect(3382.8875, 0.1, 60.0, false)
    val impulse_0     = Impulse.ar(freq = freq, phase = 0.10654729)
    val in_0          = Protect(impulse_0, -inf, inf, true)
    val hPZ1          = HPZ1.ar(in_0)
    val in_1          = Protect(hPZ1, -inf, inf, true)
    val integrator    = Integrator.ar(in_1, coeff = 0.0068862666)
    val ring3         = (3382.8875: GE) ring3 integrator
    val min_0         = ring3 min 0.10654729
    val impulse_1     = Impulse.ar(freq = 0.1, phase = 1.0)
    val min_1         = (0.0: GE) min ring3
    val times         = min_1 * 3.0
    val in_2          = Protect(min_1, -inf, inf, true)
    val pitchRatio    = Protect(impulse_1, 0.0, 4.0, false)
    val pitchShift    = PitchShift.ar(in_2, winSize = 0.001, pitchRatio = pitchRatio,
      pitchDispersion = 0.0, timeDispersion = 0.0)
    val in_3          = Protect(min_1, -inf, inf, true)
    val maxDelayTime  = Protect(0.0, 0.01, 20.0, false)
    val delayTime     = (0.0: GE) min maxDelayTime
    val allpassL      = AllpassL.ar(in_3, maxDelayTime = maxDelayTime, delayTime = delayTime,
      decayTime = 0.0)
    val min_2         = allpassL min 0.0
    val atan2         = (0.0: GE) atan2 min_1
    val in_4          = Protect(atan2, -inf, inf, true)
    val rq            = Protect(0.01, 0.01, 100.0, false)
    val resonz        = Resonz.ar(in_4, freq = 3382.8875, rq = rq)
    val dC            = DC.ar(0.8758479)
    val mix           = Mix(Seq[GE](min_0, times, pitchShift, min_2, resonz, dC))
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
