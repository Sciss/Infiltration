/*
 *  Algorithm.scala
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
import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.kollflitz.Vec
import de.sciss.lucre.expr.{DoubleVector, IntObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Copy, Folder}
import de.sciss.lucre.synth.{AudioBus, Bus, Server, Synth, Sys}
import de.sciss.nuages.NuagesView
import de.sciss.numbers
import de.sciss.kollflitz
import de.sciss.lucre.swing.LucreSwing.defer
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.Proc.{mainIn, mainOut}
import de.sciss.synth.proc.{EnvSegment, Grapheme, Output, Proc, Scheduler, SoundProcesses, TimeRef}
import de.sciss.synth.{Curve, FillValue, SynthGraph, addToTail, message}

import scala.annotation.elidable
import scala.concurrent.stm.{InTxn, Ref}
import scala.swing.Swing
import scala.util.Random

class Algorithm[S <: Sys[S], I <: Sys[I]](
                            view: NuagesView[I],
                            server            : Server,
                            localSocketAddress: InetSocketAddress,
                            config            : Config,
                            grProcH           : stm.Source[S#Tx, Grapheme[S]],
                            numProcs          : Int,
                            scheduler         : Scheduler[S],
                            surfaceH          : stm.Source[I#Tx, Folder[I]],
                            ciBal             : Int,
                          )(implicit cursor: stm.Cursor[S], /*cursorI: stm.Cursor[I],*/ bridge: S#Tx => I#Tx)
  extends /*Main with*/ SoundScene {

  private final class Fade(token: Int, startTime: Long, duration: Long,
                           val startVal: Vec[Double], val endVal: Vec[Double]) {
    private[this] val endTime = startTime + duration

    def isComplete(implicit tx: S#Tx): Boolean =
      scheduler.time >= endTime
  }

  private[this] val panel       = view.panel
  private[this] val transport   = panel.transport
  private[this] val numChannels = 4

  type ProcH = stm.Source[I#Tx, Proc[I]]

  private[this] val refFilter   = Ref(Option.empty[ProcH])
  private[this] val refGen      = Ref.make[ProcH]()
  private[this] val refFltFade  = Ref(Option.empty[Fade])

  private[this] var hndIn     : ProcH = _
  private[this] var hndOut    : ProcH = _
  private[this] var hndAdapt1 : ProcH = _

  private[this] val refAdapt1Patch = Ref(-1)  // zero-based param num
  private[this] val refGenNumParam = Ref(0)

  private[this] val refGenPar = Array.fill(5)(Ref(Vec.empty[Double]))

  private[this] implicit val random: Random = new Random()

  private[this] val refGraphPos = Ref((math.random() * numProcs).toInt)

  private[this] val showLog = config.log

  private[this] val logHeader = new SimpleDateFormat(s"[HH:mm''ss.SSS] [${config.dot}] ", Locale.US)

  @elidable(elidable.CONFIG) def log(what: => String): Unit =
    if (showLog) {
      val msg = logHeader.format(new Date()) + what
      println(msg)
      // if (remoteLogFun.isDefined) remoteLogFun.foreach(_.apply(msg))
    }

  def init()(implicit tx: S#Tx): this.type = {
    implicit val itx: I#Tx  = bridge(tx)

    tx.afterCommit {
      OscClient(this, config, localSocketAddress).init()
      Sensors  (this, config).init()
    }
    createBasicStructure()
//    deferBi { (tx, itx) => runProgram() }
    autoRunNext()

    mkAnalysisMeter(Bus.soundOut(server, numChannels = 4)) { (peak, badCnt) =>
      SoundProcesses.step[S]("peak") { implicit tx =>
        analysisUpdate(peak, badCnt)
      }
    }

    panel.setMainVolume(config.mainVolume)

//    tx.afterCommit {
//      server.peer.dumpTree(controls = true)
//    }

    this
  }

  private[this] val lowVolumeCount = Ref(0)
  private[this] val badPitchCount  = Ref(0)

  private def analysisUpdate(peak: Double, badCnt: Int)(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer

    def testThresh(c: Int, thresh1: Int, thresh2: Int, name: String): Unit = {
      if (c >= thresh1) { // start trying modifications
        log(s"too $name ($c)")
//        val thresh2 = thresh1 + 4
        val numParam = refGenNumParam()
        if (c >= thresh2 || numParam == 0) { // tried four different things without effect
          changeNegatum()
        } else {
          import numbers.Implicits._
          val parIdx = c.wrap(0, numParam - 1)
          randomizePar(parIdx)
        }
      }
    }

    if (peak > 0.05) {
      lowVolumeCount() = 0
      if (badCnt < config.badPitchCount) {
        badPitchCount() = 0
      } else {
        val c = badPitchCount.transformAndGet(_ + 1)
        testThresh(c, 3, 7, "much static pitch")
      }

    } else {
      badPitchCount() = 0
      val c = lowVolumeCount.transformAndGet(_ + 1)
      testThresh(c, 6, 11, "silent")
    }
  }

  private def removeParVec(parIdx: Int)(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer
    val r = refGenPar(parIdx)
    r()   = Vec.empty
  }

  private def putParVec(parIdx: Int, parVec: Vec[Double])(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer
    implicit val itx: I#Tx  = bridge(tx)

    val r       = refGenPar(parIdx)
    r()         = parVec
    val pGen    = refGen().apply()
    val aGen    = pGen.attr
    val parObj  = DoubleVector.newVar[I](parVec)
    aGen.put(parKey(parIdx), parObj)
  }

  private def randomizePar(parIdx: Int)(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer

    log(s"randomizePar($parIdx)")

    def perform(): Unit = {
      val v0      = Util.rangeRand(0.0, 1.0)
      val parVec  = spreadVecLin(v0)
      putParVec(parIdx, parVec)
    }

    if (refAdapt1Patch() == parIdx) {
      val numParam = refGenNumParam()
      if (numParam <= 1) return // could randomize the adaptation...
      val idx0      = Util.rand(numParam - 1)
      val adaptIdx  = if (idx0 < parIdx) idx0 else idx0 + 1
      perform()
      patchAdaptToGen(adaptIdx)

    } else {
      perform()
    }
  }

  private def mkAnalysisMeter(bus: AudioBus)(fun: (Double, Int) => Unit)(implicit tx: S#Tx): Synth = {
    val numCh  = bus.numChannels
    val graph  = {
      val res = SynthGraph {
        import de.sciss.synth._
        import Ops._
        import ugen._
        val meterTr = Impulse.kr(1.0/4) - Impulse.kr(0)
        val sig     = In.ar("in".kr, numCh)
        val peak    = Peak.kr(sig, meterTr) // .outputs
        val peakM   = Reduce.max(peak)

        val zcT     = meterTr
        val zc0     = A2K.kr(ZeroCrossing.ar(sig))
        val zc      = Median.kr(zc0, 5 /* 13 */ /* 11 */ /* 7 */)
        val zcMx    = RunningMax.kr(zc, zcT).max(1.0)
        val zcMn    = RunningMin.kr(zc, zcT).max(1.0)
        val zcRat   = zcMx / zcMn
        val zcLoRat = 0.9
        val zcHiRat = 1.0 / zcLoRat

        val isBad   = (peak > 0.1) & (zcMn > 220) & (zcMx > 220) & ((zcRat > zcLoRat) & (zcRat < zcHiRat))
        val badCnt0 = Integrator.kr(isBad, coeff = 1.0 - zcT)
        val badCnt  = Reduce.max(badCnt0)

        SendTrig.kr(meterTr, peakM, badCnt)
      }
//      peakMeterGraphMap += numCh -> res
      res
    }
//    val server  = node.server
    val syn     = Synth.play(graph, Some("analysis"))(server.defaultGroup, addAction = addToTail,
      dependencies = /*node ::*/ Nil)
    syn.read(bus -> "in")
    val NodeId = syn.peer.id
    val trigResp = message.Responder.add(server.peer) {
      case message.Trigger(NodeId, badCnt: Int, peak: Float) => defer(fun(peak, badCnt))
    }
    // Responder.add is non-transactional. Thus, if the transaction fails, we need to remove it.
    scala.concurrent.stm.Txn.afterRollback { _ =>
      trigResp.remove()
    } (tx.peer)
    syn.onEnd(trigResp.remove())
    syn
  }

  private[this] val sensorNoiseFloor    = config.sensorNoiseFloor
  private[this] val sensorTrigThreshUp  = config.sensorTrigThreshUp
  private[this] val sensorTrigThreshDn  = config.sensorTrigThreshDn

//  @volatile
//  private[this] var trigTimeA0 = 0L
//  private[this] var trigTimeA1 = 0L
//  private[this] var trigTimeA2 = 0L
//  private[this] var trigTimeA3 = 0L
//  private[this] var trigTimeB0 = 0L
//  private[this] var trigTimeB1 = 0L
//  private[this] var trigTimeB2 = 0L
//  private[this] var trigTimeB3 = 0L

  private[this] val trigStates  = new Array[Boolean ](8)
  private[this] val trigTimes   = new Array[Long    ](8)

  private[this] val maxJumpTrigSpan   = 10 * 60000L // milliseconds
  private[this] val minJumpTrigPause  = 15 * 60000L // milliseconds
  private[this] val minJumpNumTrig    = 6
  private[this] val maxFlipTrigSpan   = (config.flipTrigDurSec * 1000).toLong // 5 * 60000L // milliseconds
  private[this] val forgetDurSec      = config.forgetDurSec // 2 /*5*/ * 60

  private[this] val timeJumpGraph = Ref(0L)

  def sensorUpdate(data: Array[Float]): Unit = {
    //println(data.mkString(", "))
    var countBal  = 0
    var balBase   = 0f
    val tBal  = sensorNoiseFloor

    var ai = 0
    var bi = 4
    while (ai < 4) {
      val a = data(ai)
      val b = data(bi)
      if (a > tBal || b > tBal) {
        balBase   += a / (a + b)
        countBal += 1
      }
      ai += 1
      bi += 1
    }

    if (countBal > 0) {
      balBase /= countBal  // fully a: 1.0, fully b: 0.0
      val vBal = balBase * 2 - 1  // -1 ... +1
      //      println(s"vBal = $vBal")
      server ! message.ControlBusSet(FillValue(ciBal, vBal))
      if (config.display) Swing.onEDT {
        // let's appropriate that rotary dial, LOL
        panel.glideTime = balBase * 2 // 0 .. 1
      }
    }

    val ttUp = sensorTrigThreshUp
    val ttDn = sensorTrigThreshDn
    val ts          = trigStates
    val _trigTimes  = trigTimes
    val stamp = System.currentTimeMillis()

    ai = 0
    var hasNewTrig = false
    while (ai < 8) {
      val a = data(ai)
      if (a > ttUp) {
        if (!ts(ai)) {
          ts        (ai)  = true
          _trigTimes(ai)  = stamp
          hasNewTrig      = true
        }
      } else if (a < ttDn) {
        if (ts(ai)) {
          ts(ai) = false
        }
      }
      ai += 1
    }

    if (hasNewTrig) {
      ai = 0
      bi = 4
      val minTimeStart = stamp - maxFlipTrigSpan
      var flip = 0L
      while (ai < 4) {
        val ta = _trigTimes(ai)
        val tb = _trigTimes(bi)
        if (ts(ai) && ts(bi) && ta > minTimeStart && tb > minTimeStart /*&& ta != tb*/) {
          import numbers.Implicits._
          // we use 8 bits; zero mean no flip, otherwise 1 to 255 specifies the amount of spacing between
          // the opposite triggers
          val amt   = math.max(math.abs(ta - tb), 1000L).toDouble.expLin(
            1000.0, maxFlipTrigSpan.toDouble, 1.0, 256).toInt.clip(1, 255)
          val code  = amt.toLong << (8 * (if (ta < tb) ai else bi))
          flip |= code
        }
        ai += 1
        bi += 1
      }
      if (flip != 0) {
        SoundProcesses.step[S]("update flip") { implicit tx =>
          updateFlip(flip)
        }
      }
    }
  }

  private[this] val tkForgetFlip = Ref(-1)

  def updateFlip(code: Long)(implicit tx: S#Tx): Unit = {
    implicit val itx: I#Tx  = bridge(tx)

    log(s"update flip ${code.toHexString}") // ${(bitMask | (1 << 8)).toBinaryString.substring(1)}")

    val pAdapt1   = hndAdapt1()
    val aAdapt1   = pAdapt1.attr
    val bitMaskA  = code.toInt // (code & 0xFFFFFFFFL).toInt
    val bitMaskB  = (code >>> 32).toInt // & 0xFFFFFFFFL

    if (bitMaskA != 0) {  // flip range
      val oldLoOpt  = aAdapt1.$[DoubleVector](attrLo)
      val oldHiOpt  = aAdapt1.$[DoubleVector](attrHi)
      val oldLo     = oldLoOpt.fold(vecZero)(_.value)
      val oldHi     = oldHiOpt.fold(vecOne )(_.value)
      val (newLo, newHi) = Vector.tabulate(4) { i =>
        val amt = bitMaskA >>> (8 * i)
        val oldLoI = oldLo(i)
        val oldHiI = oldHi(i)
        if (amt == 0) (oldLoI, oldHiI) else {
          import numbers.Implicits._
          val amtLo = amt.linLin(0, 255, 0.0, 0.5)
          val amtHi = 1.0 - amtLo
          if (oldLoI < oldHiI) (amtHi, amtLo) else (amtLo, amtHi)
        }
      } .unzip
      oldLoOpt match {
        case Some(DoubleVector.Var(vr)) => vr() = newLo
        case _ => aAdapt1.put(attrLo, DoubleVector.newVar[I](newLo))
      }
      oldHiOpt match {
        case Some(DoubleVector.Var(vr)) => vr() = newHi
        case _ => aAdapt1.put(attrHi, DoubleVector.newVar[I](newHi))
      }
    }

    if (bitMaskB != 0) {  // flip mode
      val oldAbsOpt = aAdapt1.$[DoubleVector](attrAbs)
      val oldAbs    = oldAbsOpt.fold(vecZero)(_.value)
      val newAbs = Vector.tabulate(4) { i =>
        val amt = bitMaskB >>> (8 * i)
        val oldAbsI = oldAbs(i)
        if (amt == 0) oldAbsI else 1.0 - oldAbsI
      }
      oldAbsOpt match {
        case Some(DoubleVector.Var(vr)) => vr() = newAbs
        case _ => aAdapt1.put(attrLo, DoubleVector.newVar[I](newAbs))
      }
    }

    scheduleForget()
  }

  private def scheduleForget()(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer

    val timeForget = scheduler.time + (TimeRef.SampleRate * forgetDurSec).toLong
    val token = scheduler.schedule(timeForget) { implicit tx =>
      forgetFlip()
    }
    val oldToken = tkForgetFlip.swap(token)
    scheduler.cancel(oldToken)
  }

  def forgetFlip()(implicit tx: S#Tx): Unit = {
    implicit val itx: I#Tx  = bridge(tx)

    val pAdapt1   = hndAdapt1()
    val aAdapt1   = pAdapt1.attr
    val oldLoOpt  = aAdapt1.$[DoubleVector](attrLo)
    val oldHiOpt  = aAdapt1.$[DoubleVector](attrHi)
    val oldAbsOpt = aAdapt1.$[DoubleVector](attrAbs)
    val oldLo     = oldLoOpt  .fold(vecZero)(_.value)
    val oldHi     = oldHiOpt  .fold(vecOne )(_.value)
    val oldAbs    = oldAbsOpt .fold(vecZero)(_.value)
    val forgetLo  = !allSame(oldLo  )
    val forgetHi  = !allSame(oldHi  )
    val forgetAbs = !allSame(oldAbs )

    log(s"forget-flip lo $forgetLo hi $forgetHi, abs $forgetAbs")

    def meanCoin(in: Vec[Double]): Double = {
      import kollflitz.Ops._
      val mean = in.mean
      if (Util.coin(mean)) 1.0 else 0.0
    }

    def perform(oldVec: Vec[Double], coin: Double, oldOpt: Option[DoubleVector[I]], key: String): Unit = {
      val i       = oldVec.indexWhere(_ != coin)
      val newVal  = oldVec.patch(i, coin :: Nil, 1)
      oldOpt match {
        case Some(DoubleVector.Var(vr)) => vr() = newVal
        case _ => aAdapt1.put(key, DoubleVector.newVar[I](newVal))
      }
    }

    if (forgetLo || forgetHi) {
      // make sure lo and hi use opposite coins
      val coinLo = meanCoin(oldLo)
      val coinHi = 1.0 - coinLo
      if (forgetLo) perform(oldLo, coinLo, oldLoOpt, attrLo)
      if (forgetHi) perform(oldHi, coinHi, oldHiOpt, attrHi)
    }
    if (forgetAbs) {
      val coinAbs = meanCoin(oldAbs)
      perform(oldAbs, coinAbs, oldAbsOpt, attrAbs)
    }

    if (forgetLo || forgetHi || forgetAbs) {
      scheduleForget()
    }
  }

  @inline
  private def allSame(vec: Vec[Double]): Boolean = {
    val a = vec.head
    vec.forall(_ == a)
  }

  def spreadVecLin(in: Double, lo: Double = 0.0, hi: Double = 1.0): Vec[Double] = {
    import numbers.Implicits._
    val r = (hi - lo) * 0.05
    Vector.tabulate(numChannels) { ch =>
      val ta    = trigStates(ch)
      val tb    = trigStates(ch + 4)
      val rand  = if (ta ^ tb) {
        if (ta)
          Util.rangeRand(0.0, 2 * r)
        else
          Util.rangeRand(2 * r, 0.0)
      } else {
        Util.rangeRand(-r, r)
      }
      (in + rand).clip(lo, hi)
    }
  }

  def nudgeVecLin(in: Vec[Double], lo: Double = 0.0, hi: Double = 1.0): Vec[Double] = {
    import numbers.Implicits._
    in.map { v =>
      val r = (hi - lo) * 0.05
      (v + Util.rangeRand(-r, r)).clip(lo, hi)
    }
  }

  def toggleFilter()(implicit tx: S#Tx): Boolean = {
    implicit val tx0: InTxn = tx.peer
    if (refFilter().isEmpty) insertFilter() else removeFilter()
  }

  private def genOutput()(implicit tx: S#Tx): Output[I] = {
    implicit val tx0: InTxn = tx.peer
    implicit val itx: I#Tx  = bridge(tx)
    val p = refGen().apply()
    procOutput(p)
  }

//  private def outInput()(implicit tx: S#Tx): Output[I] = {
//    implicit val tx0: InTxn = tx.peer
//    implicit val itx: I#Tx  = bridge(tx)
//    val p = refGen().apply()
//    procOutput(p)
//  }

  private def procOutput(p: Proc[I])(implicit tx: S#Tx): Output[I] = {
    implicit val itx: I#Tx  = bridge(tx)
    p.outputs.add(mainOut)
  }

  private[this] val vecZero = Vec.fill(numChannels)(0.0)
  private[this] val vecOne  = Vec.fill(numChannels)(1.0)

  private[this] val attrMix = "mix"
  private[this] val attrLo  = "lo"
  private[this] val attrHi  = "hi"
  private[this] val attrAbs = "abs"

  private[this] val refTestToken = Ref(-1)

  def toggleAutoRun()(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer
    val token = refTestToken.swap(-1)
    scheduler.cancel(token)

    if (token == -1) {
      println("AUTO RUN ON")
      autoRunNext()
    } else {
      println("AUTO RUN OFF")
    }
  }

  private def autoRunNext()(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer
    val dur         = Util.rangeRand(10.0, 60.0)
    val durFrames   = (TimeRef.SampleRate * dur).toLong
    val time0       = scheduler.time
    log(s"next algorithm run in ${dur.toInt} sec.")
    val token       = scheduler.schedule(time0 + durFrames) { implicit tx =>
      autoRunAct()
    }
    refTestToken()  = token
  }

  private def autoRunAct()(implicit tx: S#Tx): Unit = {
    if (Util.coin(0.2)) {
      changeNegatum()
    } else {
      toggleFilter()
    }
    autoRunNext()
  }

  def insertFilter()(implicit tx: S#Tx): Boolean = {
    implicit val tx0: InTxn = tx.peer
    implicit val itx: I#Tx  = bridge(tx)

    refFilter().isEmpty && {
      val (name, keyFreq, keyQ) = Util.rangeRand(0, 4) match {
        case 0 => ("L-hpf", ""    , ""  )
        case 1 => ("L-lpf", ""    , ""  )
        case 2 => ("notch", "freq", "q" )
        case 3 => ("reso" , "freq", "q" )
        case 4 => ("filt" , "freq", ""  )
      }

      val n     = panel.nuages
      val fFlt  = n.filters.get

      val programOpt = for {
        pFlt0 <- fFlt.$[Proc](name)
      } yield {
        val cpy   = Copy[I, I]
        val pFlt  = cpy(pFlt0)
        cpy.finish()
        val aFlt  = pFlt.attr
        val oFlt  = pFlt.outputs.add(mainOut)

        val freq0   = Util.rangeRand(0.0, 1.0)
        val freqV   = spreadVecLin(freq0)

        val hasFreq = keyFreq.nonEmpty
        if (hasFreq) {
          val freqObj = DoubleVector.newVar[I](freqV)
          aFlt.put(keyFreq, freqObj)
        }
        if (keyQ.nonEmpty) {
          val q0 = Util.rangeRand(0.0, 0.5)
          val qV = spreadVecLin(q0, 0.0, 0.5)
          aFlt.put(keyQ, DoubleVector.newVar(qV))
        }

        val oGen = genOutput()
        aFlt.put(mainIn, oGen)
        val surface = surfaceH()
        surface.addLast(pFlt)
        val out = hndOut()
        out.attr.put(mainIn, oFlt)
        val pFltH = itx.newHandle(pFlt)
        val mixVal = if (hasFreq) vecOne else freqV

        val grMix = mkFade(refFltFade, startVal = vecZero, endVal = mixVal, dur = 10.0) {
          implicit tx =>
            fadeEnded(pFltH, attrMix, mixVal)
        }
        aFlt.put(attrMix, grMix)

        refFilter() = Some(pFltH)

        ()
      }

      val ok = programOpt.isDefined
      if (!ok) {
        println("! insertFilter failed")
      }
      ok
    }
  }

  private def fadeEnded(p: ProcH, key: String, value: Vec[Double])(implicit tx: S#Tx): Unit = {
    implicit val itx: I#Tx  = bridge(tx)
    p().attr.put(key, DoubleVector.newVar(value))
  }

  private def mkFade(fdRef: Ref[Option[Fade]], startVal: Vec[Double], endVal: Vec[Double], dur: Double)
                    (done: S#Tx => Unit /*= _ => ()*/)
                    (implicit tx: S#Tx): Grapheme[I] = {
    implicit val tx0: InTxn = tx.peer
    implicit val itx: I#Tx  = bridge(tx)

    val grMix   = Grapheme[I]()
    val pos0    = transport.position
    val time0   = scheduler.time
    val segMix0 = EnvSegment.Obj.newConst[I](EnvSegment.Multi(startVal, Curve.lin))
    grMix.add(0L, segMix0)
    grMix.add(pos0, segMix0)
    val durFrames = (TimeRef.SampleRate * dur).toLong
    val timeFd    = pos0 + durFrames
    val segMix    = DoubleVector.newConst[I](endVal)
    grMix.add(timeFd, segMix)
    val token = scheduler.schedule(time0 + durFrames) { implicit tx =>
      done(tx)
    }
    val fd = new Fade(token = token, startTime = time0, duration = durFrames, startVal = startVal, endVal = endVal)
    fdRef() = Some(fd)
    grMix
  }

  private def performRemoveFilter(pFltH: ProcH)(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer
    implicit val itx: I#Tx  = bridge(tx)

    val pFlt  = pFltH()
    val oGen  = genOutput()
    val out = hndOut()
    out.attr.put(mainIn, oGen)
    val surface = surfaceH()
    surface.remove(pFlt)
    refFilter() = None
  }

  def removeFilter()(implicit tx: S#Tx): Boolean = {
    implicit val tx0: InTxn = tx.peer
    implicit val itx: I#Tx  = bridge(tx)

    val oldFadeOpt = refFltFade()
    oldFadeOpt.forall(_.isComplete) && refFilter().exists { pFltH =>
      val startVal  = oldFadeOpt.fold(vecOne)(_.endVal)
      val pFlt      = pFltH()
      val aFlt      = pFlt.attr
      val dur       = if (Util.coin(0.2)) 1.0 else Util.rangeRand(10.0, 20.0)
      val grMix     = mkFade(refFltFade, startVal = startVal, endVal = vecZero, dur = dur) { implicit tx =>
        performRemoveFilter(pFltH)
      }
      aFlt.put(attrMix, grMix)

      true
    }
  }

  private def parKey(parIdx: Int): String = s"p${parIdx + 1}"

  private def patchAdaptToGen(parIdx: Int)(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer
    implicit val itx: I#Tx  = bridge(tx)

    val oAdapt1 = hndAdapt1().outputs.add(mainOut)
    val pGen    = refGen().apply()
    val aGen    = pGen.attr
    aGen.put(parKey(parIdx), oAdapt1)
  }

  // careful to handle the initial situation where refGen holds null
  def changeNegatum()(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer
    implicit val itx: I#Tx  = bridge(tx)

    import numbers.Implicits._
    val pIdx = {
      val stamp   = System.currentTimeMillis()
      val numTrig = trigTimes.count(tt => stamp - tt < maxJumpTrigSpan)
      if (numTrig >= minJumpNumTrig && stamp - timeJumpGraph() > minJumpTrigPause) {
        val i = Util.rand(numProcs)
        log(s"graph jump to $i")
        refGraphPos   () = i
        timeJumpGraph () = stamp
        i
      } else {
        val pAdd = Util.rangeRand(-1, 1)
        refGraphPos.transformAndGet(i => (i + pAdd).wrap(0, numProcs - 1))
      }
    }
    val grProc = grProcH()
    val pGen0SOpt = grProc.at(pIdx).flatMap { e =>
      e.value match {
        case p: Proc[S] => Some(p)
        case _          => None
      }
    }

    pGen0SOpt.foreach { pGen0S =>
      val cpy         = Copy[S, I]
      val pGenNew     = cpy(pGen0S)
      cpy.finish()

      val surface     = surfaceH()
      val pGenOldOpt  = Option(refGen.swap(itx.newHandle(pGenNew))).map(_.apply())
      val pPred       = refFilter().getOrElse(hndOut).apply()
      val oGenNew     = pGenNew.outputs.add(mainOut)
      val aGenNew     = pGenNew.attr
      val numParam    =
        if      (aGenNew.contains(parKey(4))) 5
        else if (aGenNew.contains(parKey(3))) 4
        else if (aGenNew.contains(parKey(2))) 3
        else if (aGenNew.contains(parKey(1))) 2
        else if (aGenNew.contains(parKey(0))) 1
        else 0  // huh...

      refGenNumParam() = numParam

      val patchIdx = if (numParam > 0) {
        val paramWSum = (numParam * (numParam + 1)) / 2
        val parWSeq   = 0 until numParam
        val parWFun   = (i: Int) => (numParam - i).toDouble / paramWSum
        val parIdx    = Util.weightedChoose(parWSeq)(parWFun)
        patchAdaptToGen(parIdx)
//        println(s"parIdx = $parIdx; sum ${parWSeq.map(parWFun).sum}")
        parIdx
      } else -1

      for (parIdx <- 0 until 5) {
        if (parIdx != patchIdx) {
          val oldVec = refGenPar(parIdx).apply()
          if (oldVec.isEmpty || Util.coin(0.7)) { // do not reuse old value
            if (Util.coin(0.2)) { // do not use default value; use random one
              val v0 = Util.rangeRand(0.0, 1.0)
              putParVec(parIdx, spreadVecLin(v0))

            } else {
              // ok, don't change; but save for next change
              val parObjOpt = aGenNew.$[DoubleVector](parKey(parIdx))
              val parVec    = parObjOpt.fold(Vec.empty[Double])(_.value)
              val r = refGenPar(parIdx)
              r()   = parVec
            }
          } else { // use old value
            putParVec(parIdx, nudgeVecLin(oldVec))
          }
        }
        if (parIdx >= numParam) removeParVec(parIdx)
      }

      refAdapt1Patch() = patchIdx

      pGenOldOpt.foreach(surface.remove) //  (pGenOld)
      surface.addLast (pGenNew)
      pPred.attr.put(mainIn, oGenNew)
    }
  }

  private def createBasicStructure()(implicit tx: S#Tx): Unit = {
//    implicit val tx0: InTxn = tx.peer
    implicit val itx: I#Tx  = bridge(tx)

//    val pIdx      = refGraphPos()
    val n         = panel.nuages
    val fGen      = n.generators.get
    val fFlt      = n.filters   .get
    val fCol      = n.collectors.get
//    val grProc    = grProcH()

    val programOpt = for {
      pIn0    <- fGen.$[Proc]("in")
      pAdapt0 <- fFlt.$[Proc]("adapt")
//      pGen0S  <- grProc.at(pIdx).flatMap { e =>
//        e.value match {
//          case p: Proc[S] => Some(p)
//          case _ => None
//        }
//      }
      pOut0 <- fCol.$[Proc]("O-inf" /*"O-all"*/)
    } yield {
      val cpyS    = Copy[S, I]
//      val pGen    = cpyS(pGen0S)
      cpyS.finish()
      val cpyI    = Copy[I, I]
      val pOut    = cpyI(pOut0)
      val pIn     = cpyI(pIn0)
      val pAdapt  = cpyI(pAdapt0)
      cpyI.finish()
      val aAdapt  = pAdapt.attr
      val aIn     = pIn   .attr
//      val aOut    = pOut  .attr
//      aOut  .put(mainIn, pGen.outputs.add(mainOut))
      aIn   .put("$bal-bus"   , IntObj      .newConst (ciBal))
      if (Network.hasMic(config.dot)) {
        aIn.put("gain", DoubleVector.newVar(Vec.fill(numChannels)(config.micDial)))
      }
      aAdapt.put("gain"   , DoubleVector.newVar   (Vec.fill(numChannels)(0.8)))
      aAdapt.put(mainIn   , pIn.outputs.add(mainOut))
      aAdapt.put(attrMix  , DoubleVector.newVar(vecOne))

      val surface = surfaceH()
      surface.addLast(pOut  )
//      surface.addLast(pGen  )
      surface.addLast(pIn   )
      surface.addLast(pAdapt)

//      refGen()  = itx.newHandle(pGen)
      hndOut    = itx.newHandle(pOut)
      hndIn     = itx.newHandle(pIn)
      hndAdapt1 = itx.newHandle(pAdapt)

      changeNegatum()

      ()
    }

    if (programOpt.isEmpty) {
      println("! createBasicStructure failed")
    }
  }

  private def stepI[A](f: I#Tx => A): A =
    cursor.step { implicit tx =>
      val itx: I#Tx = bridge(tx)
      f(itx)
    }

  def setMainVolume(amp: Double): Unit =
    stepI { implicit tx =>
      panel.setMainVolume(amp)
    }

  def start(): Unit =
    stepI { implicit tx =>
      panel.transport.play()
    }

  def stop(): Unit =
    stepI { implicit tx =>
      panel.transport.stop()
    }

  def testChannel(ch: Int): Boolean = {
    println("testChannel")
    false
  }

  def serverInfo(): String =
    server.peer.counts.toString
}
