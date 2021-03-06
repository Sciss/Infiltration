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

import java.net.InetSocketAddress

import de.sciss.file._
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Folder
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.{Server, Txn}
import de.sciss.mellite.Mellite
import de.sciss.nuages.{DSL, ExpWarp, IntWarp, NamedBusConfig, Nuages, ParamSpec, ParametricWarp, ScissProcs, Util, WolkenpumpeMain}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.graph.Attribute
import de.sciss.synth.proc.{AuralSystem, Durable, Grapheme, Scheduler, SoundProcesses, Universe, Workspace}
import de.sciss.synth.ugen.{ControlValues, LinXFade2}
import de.sciss.synth.{GE, proc, Server => SServer}
import org.rogach.scallop.{ArgType, ScallopConf, ValueConverter, ScallopOption => Opt}

import scala.concurrent.stm.{InTxn, Ref}
import scala.swing.{Button, Font, Insets, Label, Swing}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object Infiltration {
  final def parseSocket(s: String): Either[String, InetSocketAddress] = {
    val arr = s.split(':')
    if (arr.length != 2) Left(s"Must be of format <host>:<port>")
    else parseSocket(arr)
  }

  final protected def parseSocketDot(s: String): Either[String, (InetSocketAddress, Int)] = {
    val arr = s.split(':')
    if (arr.length != 3) Left(s"Must be of format <host>:<port>:<dot>")
    else {
      val dotS = arr(2)
      Try(dotS.toInt) match {
        case Success(dot) =>
          parseSocket(arr).map(socket => (socket,dot))
        case Failure(_) => Left(s"Invalid dot: $dotS - must be an integer")
      }
    }
  }

  private def parseSocket(arr: Array[String]): Either[String, InetSocketAddress] = {
    val host = arr(0)
    val port = arr(1)
    Try(new InetSocketAddress(host, port.toInt)) match {
      case Success(addr)  => Right(addr)
      case Failure(ex)    => Left(s"Invalid socket address: $host:$port - ${ex.getClass.getSimpleName}")
    }
  }

  final protected def validateSockets(vs: Seq[String], useDot: Boolean): Either[String, Unit] =
    vs.foldLeft(Right(()): Either[String, Unit]) { case (e, v) =>
      e.flatMap { _ =>
        val eth = if (useDot) parseSocketDot(v)
        else                  parseSocket   (v)
        eth.map(_ => ()) }
    }

  private def buildInfString(key: String): String = try {
    val clazz = Class.forName("de.sciss.infiltration.BuildInfo")
    val m     = clazz.getMethod(key)
    m.invoke(null).toString
  } catch {
    case NonFatal(_) => "?"
  }

  final def name          : String = "in|filtration"
  final def version       : String = buildInfString("version")
  final def builtAt       : String = buildInfString("builtAtString")
  final def fullVersion   : String = s"v$version, built $builtAt"
  final def nameAndVersion: String = s"$name $fullVersion"

  def main(args: Array[String]): Unit = {
    object p extends ScallopConf(args) {
      printedName = Infiltration.nameAndVersion

      private val default = Config()

      val baseDir: Opt[File] = opt(default = Some(default.baseDir),
        descr = "Base directory"
      )
      val dumpOsc: Opt[Boolean] = toggle("dump-osc",
        descrYes = "Dump OSC traffic", default = Some(default.dumpOsc),
      )
      val dumpSensors: Opt[Boolean] = toggle("dump-sensors",
        descrYes = "Dump OSC traffic from sensors", default = Some(default.dumpSensors),
      )
      val isLaptop: Opt[Boolean] = toggle("laptop",
        descrYes = "Running from laptop", default = Some(default.isLaptop),
      )
      val disableEnergySaving: Opt[Boolean] = toggle("disable-energy-saving",
        descrYes = "Disable energy saving processes", default = Some(default.disableEnergySaving),
      )
      val qjLaunch: Opt[Boolean] = toggle("qjackctl",
        descrYes = "Launch QJackCtl", default = Some(default.qjLaunch),
      )
      private implicit object InetSocketAddressConverter extends ValueConverter[InetSocketAddress] {
        def parse(s: List[(String, List[String])]): Either[String, Option[InetSocketAddress]] =
          s match {
            case (_, v :: Nil) :: Nil => parseSocket(v).map(Some(_))
            case Nil                  => Right(None)
            case _                    => Left("provide <host>:<port>")
          }

        val argType: ArgType.V = ArgType.SINGLE
      }
      val ownSocket: Opt[InetSocketAddress] = opt(
        descr = "Own IP address"
      )
      val dot: Opt[Int] = opt(default = Some(default.dot),
        descr = "Node identifier, last component of the IP address"
      )
      val log: Opt[Boolean] = toggle("log",
        descrYes = "Enable logging", default = Some(default.log),
      )
      val display: Opt[Boolean] = toggle("display",
        descrYes = "Show visual display", default = Some(default.display),
      )
      val highPass: Opt[Int] = opt("high-pass", default = Some(default.highPass),
        descr = s"High-pass cut-off frequency or zero (default ${default.highPass})"
      )
      val sensorNoiseFloor: Opt[Float] = opt("sensor-noise-floor", default = Some(default.sensorNoiseFloor),
        descr = s"Noise floor level below which sensors are interpreted as off (default: ${default.sensorNoiseFloor})"
      )
      val sensorTrigThreshUp: Opt[Float] = opt("sensor-trig-thresh-up", default = Some(default.sensorTrigThreshUp),
        descr = s"Trigger threshold for sensors (rising slope) (default: ${default.sensorTrigThreshUp})"
      )
      val sensorTrigThreshDn: Opt[Float] = opt("sensor-trig-thresh-dn", default = Some(default.sensorTrigThreshDn),
        descr = s"Trigger threshold for sensors (falling slope) (default: ${default.sensorTrigThreshDn})"
      )
      val flipTrigDurSec: Opt[Int] = opt("flip-trig-dur", default = Some(default.flipTrigDurSec),
        descr = s"Maximum duration in seconds of opposite triggers to flip adaptation (default: ${default.flipTrigDurSec})"
      )
      val forgetDurSec: Opt[Int] = opt("forget-dur", default = Some(default.forgetDurSec),
        descr = s"Duration of forgetting step in seconds (default: ${default.forgetDurSec})"
      )
      val maxMainGain: Opt[Double] = opt("max-main-gain", default = Some(default.maxMainGain),
        descr = s"Maximum main gain, linear (default: ${default.maxMainGain})"
      )
      val displayWidth: Opt[Int] = opt("display-width", default = Some(default.displayWidth),
        descr = s"Wolkenpumpe display width in pixels (default: ${default.displayWidth})"
      )
      val displayHeight: Opt[Int] = opt("display-height", default = Some(default.displayHeight),
        descr = s"Wolkenpumpe display height in pixels (default: ${default.displayHeight})"
      )
      val micDial: Opt[Double] = opt("mic-dial", default = Some(default.micDial),
        descr = s"Microphone gain dial position 0 to 1, for node 12 (default: ${default.micDial})"
      )
      val lowPass: Opt[Int] = opt("low-pass", default = Some(default.lowPass),
        descr = s"Main low-pass frequency or 0 for no low-pass (default: ${default.lowPass})"
      )
      val mainVolume: Opt[Double] = opt("main-volume", default = Some(default.mainVolume),
        descr = s"Main volume, linear (default: ${default.mainVolume})"
      )
      val badPitchCount: Opt[Int] = opt("bad-pitch-count", default = Some(default.badPitchCount),
        descr = s"Counter when bad pitch is being treated (default: ${default.badPitchCount})"
      )

      verify()

      val config: Config = Config(
        baseDir             = baseDir(),
        dumpOsc             = dumpOsc(),
        dumpSensors         = dumpSensors(),
        isLaptop            = isLaptop(),
        disableEnergySaving = disableEnergySaving(),
        qjLaunch            = qjLaunch(),
        ownSocket           = ownSocket.toOption,
        dot                 = dot(),
        log                 = log(),
        display             = display(),
        highPass            = highPass(),
        sensorNoiseFloor    = sensorNoiseFloor(),
        sensorTrigThreshUp  = sensorTrigThreshUp(),
        sensorTrigThreshDn  = sensorTrigThreshDn(),
        flipTrigDurSec      = flipTrigDurSec(),
        forgetDurSec        = forgetDurSec(),
        maxMainGain         = maxMainGain(),
        displayWidth        = displayWidth(),
        displayHeight       = displayHeight(),
        micDial             = micDial(),
        lowPass             = lowPass(),
        mainVolume          = mainVolume(),
        badPitchCount       = badPitchCount(),
      )
    }

    val cfg0  = p.config
    println(p.printedName)
    val localSocketAddress = Network.initConfig(cfg0)
    val cfg   = if (cfg0.dot >= 0) cfg0 else cfg0.copy(dot = Network.resolveDot(cfg0, localSocketAddress))

//    if (cfg.log) main.showLog = true

    if (!cfg.isLaptop && cfg.qjLaunch) {
      // -p preset, -a active patch bay, -s start server
      val cmd = Seq("qjackctl") // , "-p", cfg.qjPreset, "-a", cfg.qjPatchBay.path, "-s")
      println(cmd.mkString(" "))
      import sys.process._
      try {
        cmd.run()
      } catch {
        case NonFatal(ex) =>
          Console.err.println("Could not start QJackCtl")
          ex.printStackTrace()
      }
    }

//    Wolkenpumpe.init()
    Mellite .initTypes()
    run(localSocketAddress, cfg)
  }

  def any2stringadd(in: Any): Any = ()

  def deviceName: String = "Infiltration"

  def run(localSocketAddress: InetSocketAddress, config: Config): Unit = {
    type S = Durable
    type I = S#I

    val dbCfg         = BerkeleyDB.Config()
    dbCfg.readOnly    = true
    dbCfg.allowCreate = false
    val trunkId       = Network.mapDotToTrunk(config.dot)
    val wsDir         = config.baseDir / "workspaces" / s"Trunk${trunkId}graph.mllt"
    val dbF           = BerkeleyDB        .factory(wsDir, dbCfg)
    val ws            = Workspace.Durable .read   (wsDir, dbF)
    val rowIdx        = (Network.dotSeqCtl.indexOf(config.dot) + 1) * 2

    implicit val system: S = ws.system // InMemory()

    val w: WolkenpumpeMain[I] = new WolkenpumpeMain[I] {
      override protected def configure(sCfg: ScissProcs.ConfigBuilder, nCfg: Nuages.ConfigBuilder,
                                       aCfg: SServer.ConfigBuilder): Unit = {
        super.configure(sCfg, nCfg, aCfg)
        sCfg.genNumChannels = 4
        nCfg.mainChannels   = Some(0 until 4)
        nCfg.soloChannels   = None
        // we need one here so that the right number of input channels is chosen
//        nCfg.lineInputs     = Vector(NamedBusConfig("ignore", 0 until 2))
        nCfg.lineInputs     = Vector(NamedBusConfig("ignore", 0 until 1), NamedBusConfig("ignore", 1 until 2))
        nCfg.lineOutputs    = Vector(NamedBusConfig("network" /* "ignore"*/, 4 until 6))
        nCfg.micInputs      = Vector.empty
        nCfg.lineOutputsSplay = false
//        nCfg.mainSynth      = false
        nCfg.mainSynth      = true
        nCfg.showTransport  = false
        nCfg.showFrame      = config.display
        nCfg.meters         = config.display
        nCfg.displaySize    = (config.displayWidth, config.displayHeight)
        sCfg.highPass       = config.highPass
        aCfg.deviceName     = Some(deviceName)
      }

      override protected def registerProcesses(nuages: Nuages[I], nCfg: Nuages.Config, sCfg: ScissProcs.Config)
                                              (implicit tx: I#Tx, universe: Universe[I]): Unit = {
        super.registerProcesses(nuages, nCfg, sCfg)

        val dsl = DSL[I]
        import dsl._
        import sCfg.genNumChannels

        //        val mainChansOption = nCfg.mainChannels

        def ForceChan(in: GE): GE = if (genNumChannels <= 0) in else {
          Util.wrapExtendChannels(genNumChannels, in)
        }

        implicit val _nuages: Nuages[I] = nuages

        def filterF   (name: String)(fun: GE => GE): proc.Proc[I] =
          filter      (name, if (DSL.useScanFixed) genNumChannels else -1)(fun)

        def collectorF(name: String)(fun: GE => Unit): proc.Proc[I] =
          collector   (name, if (DSL.useScanFixed) genNumChannels else -1)(fun)

        def mix(in: GE, flt: GE, mix: GE): GE = LinXFade2.ar(in, flt, mix * 2 - 1)
        def mkMix(df: Double = 0.0): GE = pAudio("mix", ParamSpec(0, 1), default(df))

        def default(in: Double): ControlValues =
          if (genNumChannels <= 0)
            in
          else
            Vector.fill(genNumChannels)(in)

        filterF("adapt") { in =>
          import de.sciss.synth.ugen._

          val pLo   = pAudio("lo"   , ParamSpec(0.0, 1.0), default(0.0))
          val pHi   = pAudio("hi"   , ParamSpec(0.0, 1.0), default(1.0))
          val pRect = pAudio("abs"  , ParamSpec(0.0, 1.0, IntWarp), default(0.0))
          val pGain = pAudio("gain" , ParamSpec(-30, 30), default(0.0))
          val pMix  = mkMix()
          val inG   = in * pGain.dbAmp
//          val flt1  = inG.linLin(-1.0, 1.0, pLo, pHi)
//          val flt2  = inG.abs.linLin( 0.0, 1.0, pLo, pHi)
          val flt1  = inG.max(0.0)
          val flt2  = inG.abs
          val flt3  = Select.ar(pRect, flt1 :: flt2 :: Nil)
          val flt   = flt3 * (pHi - pLo) + pLo
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
          val pBoost  = pAudio("gain" , ParamSpec( 0.1, 10, ExpWarp), default(1.0))
//          val pBal    = pAudio("bal"  , ParamSpec(-1.0, 1.0, LinWarp), default(0.0))
          val sig0    = In.ar(NumOutputBuses.ir, 2)
          val sig     = LeakDC.ar(sig0)
          val sigL    = sig.out(0)
          val sigR    = sig.out(1)
//          val vBal    = pBal.out(0)
          // we can't get a direct n_map, so let's supply the bus as argument
          val vBalBus = Attribute.ar("$bal-bus", 0.0, fixed = true)
          val vBal    = In.kr(vBalBus)
//          vBal.poll(2, "vBal")
          val vBoost  = pBoost.out(0)
          val balance = Balance2.ar(sigL, sigR, pos = vBal, level = vBoost)
          val sum     = balance.left + balance.right
//          val buf     = LocalBuf(1024, 1) // WARNING: must be 1024 for Loudness
//          val fft     = FFT(buf, sum, hop = 1.0, winType = 1)
//          val loud    = Loudness.kr(fft)
//          loud.poll(1, "loud")
          val sig1: GE = ForceChan(sum)
          sig1
        }

//        def mkDirectOut(sig0: GE): Unit = {
//          val bad = CheckBadValues.ar(sig0)
//          val sig = Gate.ar(sig0, bad sig_== 0)
//          nCfg.mainChannels.get.zipWithIndex.foreach { case (ch, i) =>
//            val sig0 = sig out i
//            val hpf  = sCfg.highPass
//            val sig1 = if (hpf >= 16 && hpf < 20000) HPF.ar(sig0, hpf) else sig0
//            Out.ar(ch, sig1)   // XXX TODO - should go to a bus w/ limiter
//          }
//        }

        collectorF("O-inf") { in0 =>
          import de.sciss.synth.ugen._
          val bad = CheckBadValues.ar(in0)
          val in  = Gate.ar(in0, bad sig_== 0)
          val inM = Mix.mono(in)
//          CheckBadValues.ar(inM)
          val b1  = BPF.ar(inM, freq =  333, rq = 1)
          val b2  = BPF.ar(inM, freq = 1000, rq = 1)
          val b3  = BPF.ar(inM, freq = 3000, rq = 1)
          val r1  = Decay.kr(b1.abs, 0.2)
          val r2  = Decay.kr(b2.abs, 0.2)
          val r3  = Decay.kr(b3.abs, 0.2)
          //          r1.ampDb.poll(1, "r1")
          //          r2.ampDb.poll(1, "r2")
          //          r3.ampDb.poll(1, "r3")
          val gainF = ((r1 + 3 * r2 + 2 * r3).reciprocal * 1.25 /* * 10*/).min(config.maxMainGain /*1.0*/ /*8.0*/ /*4.0*/)
          val gain = LagUD.kr(gainF, timeUp = 2.0, timeDown = 0.1)
          //val gain = Lag.kr(gainF, 1.0).min(4.0)
          //          gain.ampDb.poll(1, "gain")
          val inG = in * gain
          val sig = inG // Limiter.ar(inG)
          //          ReplaceOut.ar(0, sig) // in0 /* DelayN.ar(in0, 0.2, 0.2) */ * gain)
          //          Out.ar(0, sig)
//          mkDirectOut(sig)
          //r1.poll(1, "r1")
          //r2.poll(1, "r2")
//          in.poll(2, "IN")
          nCfg.mainChannels.get.zipWithIndex.foreach { case (ch, i) =>
            val sig0 = sig out i
            val hpf  = sCfg.highPass
            val sig1 = if (hpf >= 16 && hpf < 20000) HPF.ar(sig0, hpf) else sig0
            val sig2 = if (config.lowPass > 0) LPF.ar(sig1, config.lowPass) else sig1
//            sig1.poll(2, s"sig[$ch]")
            Out.ar(ch, sig2)   // XXX TODO - should go to a bus w/ limiter
          }
        }
      }
    }

    val (nuagesH: stm.Source[I#Tx, Nuages[I]], prGraphH: stm.Source[S#Tx, Grapheme[S]], numProcs) =
      system.step { implicit tx =>
        implicit val itx: I#Tx = system.inMemoryTx(tx)

        val circleName = "circle"
        val r         = ws.root
        val grapheme  = r.$[Grapheme](circleName).getOrElse(sys.error(s"No grapheme '$circleName' found"))
        val _numProcs = grapheme.lastEvent.getOrElse(sys.error("Huh, empty?")).toInt
        println(s"numProcs = ${_numProcs}")
        (itx.newHandle(Nuages.folder[I]), tx.newHandle(grapheme), _numProcs)
      }

    implicit val systemI: I = system.inMemory
    w.run(nuagesH)
    val view  = w.view

    val infR = Ref(Option.empty[Algorithm[S, I]])

    val panel = view.panel
    Swing.onEDT {
      panel.display.setHighQuality(false)

      w.frame.foreach { f =>
        f.frame.title = s"$nameAndVersion - .${config.dot}"
      }

      def button(name: String)(fun: => Unit): Unit = {
        val b = Button(name)(fun)
        b.margin = new Insets(2, 2, 2, 2)
        view.addSouthComponent(b)
      }

      def buttonTx(name: String)(fun: S#Tx => Unit): Unit =
        button(name)(system.step(fun(_)))

      button("Sta") {
        val sOpt = system.step { implicit tx =>
          w.auralSystem.serverOption
        }
        sOpt.foreach { s =>
          s.peer.dumpTree(controls = true)
          println(s.counts)
        }
      }

      buttonTx("Cha") { implicit tx =>
        implicit val tx0: InTxn = tx.peer
        infR().foreach(_.changeNegatum())
      }

//      buttonTx("Flt") { implicit tx =>
//        implicit val tx0: InTxn = tx.peer
//        infR().foreach(_.toggleFilter())
//      }

      buttonTx("Run") { implicit tx =>
        implicit val tx0: InTxn = tx.peer
        infR().foreach(_.toggleAutoRun())
      }

      view.addSouthComponent(new Label(rowIdx.toString) {
        font = Font(Font.SansSerif, Font.Plain, 36)
      })
    }

    system.step { implicit tx =>
      w.auralSystem.addClientNow(new AuralSystem.Client {
        def auralStarted(server: Server)(implicit tx: Txn): Unit = {
//          val dfPostM = SynthGraph {
//            import de.sciss.synth._
//            import de.sciss.synth.ugen._
////            val nConfig     = panel.config
//            import Ops._
//            val amp   = "amp".kr(1f)
//            val in0   = In.ar(0, 4)
//            val in  = Mix.mono(in0)
//            CheckBadValues.ar(in)
//            val b1  = BPF.ar(in, freq =  333, rq = 1)
//            val b2  = BPF.ar(in, freq = 1000, rq = 1)
//            val b3  = BPF.ar(in, freq = 3000, rq = 1)
//            val r1  = Decay.kr(b1.abs, 0.2)
//            val r2  = Decay.kr(b2.abs, 0.2)
//            val r3  = Decay.kr(b3.abs, 0.2)
////            r1.ampDb.poll(1, "r1")
////            r2.ampDb.poll(1, "r2")
////            r3.ampDb.poll(1, "r3")
//            val gainF = ((r1 + 3 * r2 + 2 * r3).reciprocal * 10).min(8.0 /*4.0*/)
//            val gain = LagUD.kr(gainF, timeUp = 2.0, timeDown = 0.1) * amp
////            gain.ampDb.poll(1, "gain")
//            val sig = Limiter.ar(in0 * gain)
//            ReplaceOut.ar(0, sig)
//          }
//          val synPostM = Synth.play(dfPostM, Some("post-main"))(server.defaultGroup, addAction = addAfter)
//          panel.mainSynth = Some(synPostM)

          val ciBal = server.allocControlBus(1)
          // server ! message.ControlBusSet(FillValue(ciBal, 0.0f))  // init

          tx.afterCommit {
            SoundProcesses.step[S]("init infiltration") { implicit tx =>
              implicit val tx0: InTxn = tx.peer
              implicit val itx: I#Tx  = tx.inMemory
              val surface = nuagesH().surface.peer.asInstanceOf[Folder[I]]
              val inf = new Algorithm[S, I](
                view,
                server    = server,
                localSocketAddress = localSocketAddress,
                config    = config,
                grProcH   = prGraphH,
                numProcs  = numProcs,
                scheduler = Scheduler[S](),
                surfaceH  = itx.newHandle(surface),
                ciBal     = ciBal,
              ).init()

              infR() = Some(inf)
            }
          }
        }

        def auralStopped()(implicit tx: Txn): Unit = ()
      })
    }
  }
}
