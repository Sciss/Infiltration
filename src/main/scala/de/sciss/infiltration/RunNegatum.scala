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
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{Folder, Sys}
import de.sciss.mellite.{Application, Mellite}
import de.sciss.negatum.impl.{NegatumRenderingImpl, ParamRanges, UGens}
import de.sciss.negatum.{Negatum, Optimize, Rendering}
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{AudioCue, Durable, Proc, Universe, Workspace}
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object RunNegatum {
  final case class Config(workspace       : File,
                          template        : File,
                          startFrame      : Int,
                          endFrame        : Int,
                          optimizeInterval: Int,
                          optimizeResults : Boolean,
                          genPop          : Int,
                          probMut         : Double,
                          probDefault     : Double,
                          evalTimeout     : Double,
                          storeBadDefs    : Boolean,
                         ) {
    def formatTemplate(frame: Int): File = {
      template.replaceName(template.name.format(frame))
    }
  }

  def main(args: Array[String]): Unit = {
    // File.createTemp(deleteOnExit = false)
    object p extends ScallopConf(args) {
      printedName = "in|filtration"

      val workspace: Opt[File] = opt(required = true,
        descr = "Workspace to create and populate"
      )
      val template: Opt[File] = opt(required = true,
        descr = "Template input sound file where %d is the frame place holder"
      )
      val startFrame: Opt[Int] = opt("start-frame", validate = _ >= 0, default = Some(1),
        descr = "Start frame index"
      )
      val endFrame: Opt[Int] = opt("end-frame", required = true, validate = _ >= 0,
        descr = "End frame index (inclusive)"
      )
      val optimizeInterval: Opt[Int] = opt("optimize-interval", validate = _ > 0, default = Some(36),
        descr = "Optimization interval in frames"
      )
      val genPop: Opt[Int] = opt("gen-population", validate = _ > 0, default = Some(100),
        descr = "Generation population size"
      )
      val probMut: Opt[Double] = opt("prob-mutation", validate = i => i >= 0.0 && i <= 1.0, default = Some(0.75),
        descr = "Probability of mutation (0-1)"
      )
      val probDefault: Opt[Double] = opt("prob-default",
        validate = i => i >= 0.0 && i <= 1.0, default = Some(0.001),
        descr = "Probability of using default parameters (0-1)"
      )
      val evalTimeout: Opt[Double] = opt("eval-timeout",
        validate = _ > 0.0, default = Some(20.0),
        descr = "Time-out in seconds for sound evaluation"
      )
      val optimizeResults: Opt[Boolean] = toggle("optimize-results",
        descrYes = "Optimize results before storing", default = Some(false),
      )
      val storeBadDefs: Opt[Boolean] = toggle("store-bad-definitions",
        descrYes = "Store bad synth definitions in user home", default = Some(false),
      )

      verify()
      val config: Config = Config(
        workspace         = workspace(),
        template          = template(),
        startFrame        = startFrame(),
        endFrame          = endFrame(),
        optimizeInterval  = optimizeInterval(),
        genPop            = genPop(),
        probMut           = probMut(),
        probDefault       = probDefault(),
        evalTimeout       = evalTimeout(),
        optimizeResults   = optimizeResults(),
        storeBadDefs      = storeBadDefs(),
      )
    }

    val config  = p.config
    val wsDir   = config.workspace

    Application.init(Mellite)
    Mellite.initTypes()
    NegatumRenderingImpl.REPORT_TIME_OUT        = true
    NegatumRenderingImpl.STORE_BAD_DEFINITIONS  = config.storeBadDefs

    type S = Durable

    val (ws: Workspace[S], frame: Int, nH: stm.Source[S#Tx, Negatum[S]]) = if (wsDir.exists()) {
      val dsf = BerkeleyDB.factory(wsDir, createIfNecessary = false)
      val _ws = Workspace.Durable.read(wsDir, dsf)
      val (__frame, __nH) = _ws.system.step { implicit tx =>
        val n = _ws.root.get(0) match {
          case Some(n: Negatum[S]) => n
          case _ => sys.error("Did not find 'Negatum' as first object of workspace")
        }
        val _frame = n.attr.$[IntObj]("frame").fold(config.startFrame)(_.value)
        (_frame, tx.newHandle(n))
      }
      (_ws, __frame, __nH)

    } else {
      val dsf = BerkeleyDB.factory(wsDir, createIfNecessary = true)
      val _ws = Workspace.Durable.empty(wsDir, dsf)
      val _nH = init(_ws, config)
      (_ws, config.startFrame, _nH)
    }

    implicit val system: S = ws.system
    implicit val universe: Universe[S] = system.step { implicit tx => Universe.dummy[S] }

    val syncQuit = waitForQuit()

    tweak()

    iterate[S /*, system.I*/](ws, config, nH, frame = frame, syncQuit = syncQuit)
  }

  def tweak(): Unit = {
    ParamRanges.map = ParamRanges.map.transform {
      case ("GVerb", info) =>
        info.copy(params = info.params ++ Map(
          "roomSize"      -> ParamRanges.Spec(lo = 0.95, lessThan = Some("maxRoomSize")),  // lo!
          "maxRoomSize"   -> ParamRanges.Spec(lo = 0.95, hi = 300.0 /* soft */, scalar = true)
        ))

      case ("Blip", info) =>
        info.copy(params = info.params ++ Map(
          "freq"    -> ParamRanges.Spec(lo = 10.0, hi = 60.0 /* ! */),
          "numHarm" -> ParamRanges.Spec(lo = 1.0)
        ))

      case ("Impulse", info) =>
        info.copy(params = info.params ++ Map(
          "freq"    -> ParamRanges.Spec(lo = 0.1, hi = 60.0 /* ! */),
          "phase"   -> ParamRanges.Spec(lo = 0.0, hi = 1.0)
      ))

      case (_, other) => other
    }

//    println(ParamRanges.map("GVerb"))
  }

  def init[S <: Sys[S]](ws: Workspace[S], config: Config): stm.Source[S#Tx, Negatum[S]] = {
    val fStart = config.formatTemplate(config.startFrame)
    val specStart = AudioFile.readSpec(fStart)

    ws.cursor.step { implicit tx =>
      val cueStart = AudioCue(fStart, specStart, offset = 0L, gain = 1.0)
      val cueStartObj = AudioCue.Obj.newConst[S](cueStart)

      val n = Negatum[S](cueStartObj)
      n.name = "Negatum"
      ws.root.addLast(n)
      tx.newHandle(n)
    }
  }

  lazy val UGenExclude: Set[String] = Set(
    "CuspL", "CuspN",
    "FSinOsc",
    "GbmanL", "GbmanN",
    "HenonC", "HenonL", "HenonN",
    "LatoocarfianC", "LatoocarfianL", "LatoocarfianN",
    "LFCub", "LFGauss", "LFPar", "LFTri", "LFSaw",
    "LinCongC", "LinCongL", "LinCongN",
    "LorenzL",
    "Pulse",
    "QuadC", "QuadL", "QuadN",
    "Saw", "SinOsc",
    "StandardL", "StandardN",
    "SyncSaw", "VarSaw",
  )

  lazy val UGenNames: Set[String] = UGens.map.keySet -- UGenExclude

  def iterate[S <: Sys[S]](ws: Workspace[S], config: Config, nH: stm.Source[S#Tx, Negatum[S]], frame: Int,
                           syncQuit: AnyRef)
                          (implicit universe: Universe[S], system: S): Unit = {

    val fStart    = config.formatTemplate(frame)
    val specStart = AudioFile.readSpec(fStart)

    println(s"Iteration $frame")
    println("_" * 100)

    def abort(tr: Try[Any]): Unit = {
      println()
      println(tr)
      tr match {
        case Failure(ex) =>
          ex.printStackTrace()
        case _ =>
      }
      ws.close()
      syncQuit.synchronized {
        syncQuit.notifyAll()
      }
    }

    import Mellite.executionContext
    import universe.cursor

    /*val rendering =*/ cursor.step { implicit tx =>
      val cueValue  = AudioCue(fStart, specStart, offset = 0L, gain = 1.0)
      val cueObj    = AudioCue.Obj.newConst[S](cueValue)

      val n = nH()
      n.template() = cueObj

      val generation  = Negatum.Generation(
        population      = config.genPop, // 1000,
        probConst       = 0.5,
        minVertices     = 32,
        maxVertices     = 128,
        probDefault     = config.probDefault, // 0.05,
        allowedUGens    = UGenNames,
      )
      val evaluation  = Negatum.Evaluation.apply2(
        minFreq         = 100,
        maxFreq         = 16000,
        numMel          = 42,
        numMFCC         = 32,
        maxBoost        = 10.0,
        timeWeight      = 0.5,
        timeOut         = config.evalTimeout,
      )
      val penalty     = Negatum.Penalty()
      val breeding    = Negatum.Breeding(
        selectFraction  = 0.33,
        elitism         = 3,
        minMut          = 2,
        maxMut          = 5,
        probMut         = config.probMut,
        golem           = 15,
      )
      val nCfg        = Negatum.Config(
        seed            = System.currentTimeMillis(),
        generation      = generation,
        evaluation      = evaluation,
        penalty         = penalty,
        breeding        = breeding,
      )

      val r = n.run(nCfg)

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

            type Candidate  = (stm.Source[S#Tx, Proc[S]], SynthGraph, Boolean)
            type Candidates = Vec[Candidate]

            val cand: Candidates =
              n.population.iterator.collect {
                case p: Proc[S] if p.attr.$[DoubleObj](Negatum.attrFitness).exists(_.value > 0.0) =>
                  (tx.newHandle(p), p.graph.value, false)
              } .toIndexedSeq

            tx.afterCommit {
              val optPop = frame % config.optimizeInterval == 0
              val runOpt = config.optimizeResults || optPop

              def loop(rem: Candidates, res: Candidates): Future[Candidates] = rem match {
                case head +: tail =>
                  // we cannot expand things, because we replace the individual in the Negatum object,
                  // and otherwise next iteration wouldn't work
                  val oCfg = Optimize.Config(
                    graph         = head._2,
                    sampleRate    = specStart.sampleRate,
                    analysisDur   = specStart.numFrames / specStart.sampleRate,
                    blockSize     = 64,
                    expandProtect = false, // true,
                    expandIO      = false, // true,
                  )
                  val o = Optimize(oCfg)
                  o.start()
                  val ot: Future[Candidate] = o.transform {
                    case Success(oRes) =>
                      Success((head._1, oRes.graph, true): Candidate)
                    case Failure(_) =>
                      Success(head)
                  }

                  ot.flatMap { out =>
                    loop(tail, res :+ out)
                  }

                case _ => Future.successful(res)
              }

              def applyOpt(vec: Candidates): Unit = {
                val nextFrame = frame + 1

                var problems = 0
                cursor.step { implicit tx =>
                  val f = Folder[S]()
                  f.name = s"It $frame"
                  vec.foreach { case (pH, g, replace) =>
                    val p       = pH()
                    if (replace && optPop) p.graph() = g
                    val pC      = Proc[S]()
                    pC.graph()  = g
                    pC.name     = p.name
                    p.attr.get(Negatum.attrFitness).foreach { fit =>
                      pC.attr.put(Negatum.attrFitness, fit)
                    }
                    pC.attr.put("optimized", BooleanObj.newConst(replace))
                    if (!replace && config.optimizeResults) {
                      // keep track of the problems
                      problems +=1
                    }
                    f.addLast(pC)
                  }
                  ws.root.addLast(f)
                  val n = nH()
                  n.attr.put("frame", IntObj.newConst(nextFrame))
                }
                if (problems > 0) {
                  println(s"- There were problems ($problems) with optimization")
                }

                if (nextFrame < config.endFrame) {
                  iterate(ws, config, nH, frame = nextFrame, syncQuit = syncQuit)
                } else {
                  abort(tr)
                }
              }

              println()
              if (runOpt) {
                println(s"Optimizing ${cand.size} candidates...")
                val futOpt = loop(cand, Vector.empty)
                futOpt.onComplete {
                  case Success(vec) =>
                    applyOpt(vec)

                  //                case fail @ Failure(_: MkSynthGraph.Incomplete) =>
                  //                  println(fail)
                  //                  applyOpt(cand, replace = false)

                  case tr => abort(tr)
                }
              } else {
                println(s"Found ${cand.size} candidates.")
                applyOpt(cand)
              }
            }

          } else {
            tx.afterCommit {
              abort(tr)
            }
          }

        case _ =>
      }
      }

      () // r
    }
  }

  private def waitForQuit(): AnyRef = {
    val syncQuit = new AnyRef
    new Thread("wait-for-termination") {
      override def run(): Unit =
        syncQuit.synchronized {
          syncQuit.wait()
          println("Done.")
          sys.exit()
        }

      start()
    }
    syncQuit
  }
}
