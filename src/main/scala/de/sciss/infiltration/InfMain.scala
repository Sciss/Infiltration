/*
 *  InfMain.scala
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

import java.awt.geom.Point2D
import java.net.InetSocketAddress

import de.sciss.kollflitz.Vec
import de.sciss.lucre.expr.{DoubleVector, IntObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Copy, Folder, Obj}
import de.sciss.lucre.synth.{Server, Sys}
import de.sciss.synth.{Curve, FillValue, message}
import de.sciss.nuages.NuagesView
import de.sciss.numbers
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{EnvSegment, Grapheme, Output, Proc, Scheduler, TimeRef}

import scala.concurrent.stm.{InTxn, Ref}
import scala.swing.Swing
import scala.util.Random

class InfMain[S <: Sys[S], I <: Sys[I]](
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

  private[this] var hndIn : ProcH = _
  private[this] var hndOut: ProcH = _

  private[this] implicit val random: Random = new Random()

  private[this] val refGraphPos = Ref((math.random() * numProcs).toInt)

  def init()(implicit tx: S#Tx): this.type = {
    tx.afterCommit {
      OscClient(this, config, localSocketAddress).init()
      Sensors  (this, config).init()
    }
    createBasicStructure()
//    deferBi { (tx, itx) => runProgram() }
    this
  }

  private[this] val sensorThresh = config.sensorNoiseFloor

  def sensorUpdate(data: Array[Float]): Unit = {
    //println(data.mkString(", "))
    var count = 0
    var dir   = 0f
    val t     = sensorThresh
    val a0    = data(0)
    val a1    = data(1)
    val a2    = data(2)
    val a3    = data(3)
    val b0    = data(4)
    val b1    = data(5)
    val b2    = data(6)
    val b3    = data(7)
    if (a0 > t || b0 > t) {
      dir   += a0 / (a0 + b0)
      count += 1
    }
    if (a1 > t || b1 > t) {
      dir   += a1 / (a1 + b1)
      count += 1
    }
    if (a2 > t || b2 > t) {
      dir   += a2 / (a2 + b2)
      count += 1
    }
    if (a3 > t || b3 > t) {
      dir   += a3 / (a3 + b3)
      count += 1
    }
    if (count > 0) {
      dir /= count  // fully a: 1.0, fully b: 0.0
      val vBal = dir * 2 - 1  // -1 ... +1
//      println(s"vBal = $vBal")
      server ! message.ControlBusSet(FillValue(ciBal, vBal))
      if (config.display) Swing.onEDT {
        // let's appropriate that rotary dial, LOL
        panel.glideTime = dir * 2 // 0 .. 1
      }
    }
  }

  def spreadVecLin(in: Double, lo: Double, hi: Double): Vec[Double] = {
    import numbers.Implicits._
    val r = (hi - lo) * 0.05
    Vector.fill(numChannels)((in + Util.rangeRand(-r, r)).clip(lo, hi))
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
    p.outputs.add(Proc.mainOut)
  }

  private[this] val vecZero = Vec.fill(numChannels)(0.0)
  private[this] val vecOne  = Vec.fill(numChannels)(1.0)

  private[this] val attrMix = "mix"

  private[this] val refTestToken = Ref(-1)

  def toggleTestRun()(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer
    val token = refTestToken.swap(-1)
    scheduler.cancel(token)

    if (token == -1) {
      println("TEST RUN ON")
      testRunNext()
    } else {
      println("TEST RUN OFF")
    }
  }

  private def testRunNext()(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer
    val dur         = Util.rangeRand(10.0, 60.0)
    val durFrames   = (TimeRef.SampleRate * dur).toLong
    val time0       = scheduler.time
    println(s"TEST RUN DUR $dur")
    val token       = scheduler.schedule(time0 + durFrames) { implicit tx =>
      testRunAct()
    }
    refTestToken()  = token
  }

  private def testRunAct()(implicit tx: S#Tx): Unit = {
    if (Util.coin(0.2)) {
      changeNegatum()
    } else {
      toggleFilter()
    }
    testRunNext()
  }

  def insertFilter()(implicit tx: S#Tx): Boolean = {
    implicit val tx0: InTxn = tx.peer
    implicit val itx: I#Tx  = bridge(tx)

    (refFilter().isEmpty) && {
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
        val oFlt  = pFlt.outputs.add(Proc.mainOut)

        val freq0   = Util.rangeRand(0.0, 1.0)
        val freqV   = spreadVecLin(freq0, 0.0, 1.0)

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
        aFlt.put(Proc.mainIn, oGen)
        val surface = surfaceH()
        surface.addLast(pFlt)
        val out = hndOut()
        out.attr.put(Proc.mainIn, oFlt)
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
    out.attr.put(Proc.mainIn, oGen)
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
      val grMix     = mkFade(refFltFade, startVal = startVal, endVal = vecZero, dur = 10.0) { implicit tx =>
        performRemoveFilter(pFltH)
      }
      aFlt.put(attrMix, grMix)

      true
    }
  }

  def changeNegatum()(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer
    implicit val itx: I#Tx  = bridge(tx)

    import numbers.Implicits._
    val pAdd    = Util.rangeRand(-1, 1) // util.Random.nextInt(3) - 1
    val pIdx    = refGraphPos.transformAndGet(i => (i + pAdd).wrap(0, numProcs - 1))
    val grProc  = grProcH()
    val pGen0SOpt  = grProc.at(pIdx).flatMap { e =>
      e.value match {
        case p: Proc[S] => Some(p)
        case _          => None
      }
    }

    pGen0SOpt.foreach { pGen0S =>
      val cpy       = Copy[S, I]
      val pGenNew   = cpy(pGen0S)
      cpy.finish()

      val surface   = surfaceH()
      val pGenOld   = refGen.swap(itx.newHandle(pGenNew)).apply()
      val pPred     = refFilter().getOrElse(hndOut).apply()
      val oGenNew   = pGenNew.outputs.add(Proc.mainOut)

      surface.remove  (pGenOld)
      surface.addLast (pGenNew)
      pPred.attr.put(Proc.mainIn, oGenNew)
    }
  }

  def changeNegatumOLD()(implicit tx: S#Tx): Unit = {
    implicit val itx: I#Tx = bridge(tx)
    val nOpt = panel.nodes.find(_.name.startsWith("n-"))
    nOpt.fold[Unit](println("Ooops. Not found")) { nGenOld =>
      val pIdx = (math.random() * numProcs).toInt
      val grProc  = grProcH()
      val pGen0SOpt  = grProc.at(pIdx).flatMap { e =>
        e.value match {
          case p: Proc[S] => Some(p)
          case _ => None
        }
      }

      pGen0SOpt.foreach { pGen0S =>
//        val pOld = nGenOld.obj

        val mapOut: Map[String, Set[(Obj.AttrMap[I], String)]] = nGenOld.outputs.map { case (keyOut, nOutOld) =>
          val outOld  = nOutOld.output
          val set = nOutOld.mappings.flatMap { nAttrIn =>
            val nAttr = nAttrIn.attribute
            val keyIn = nAttr.key
            val pCol  = nAttr.parent.obj //  nAttrIn.input
            val aCol  = pCol.attr
            aCol.get(keyIn) match {
              case Some(f: Folder[I]) if f.iterator.contains(outOld) =>
//                if (f.size > 1) f.remove(outOld) else aCol.remove(keyIn)
                Some((aCol, keyIn))

              case Some(o: Output[I]) if o == outOld =>
//                aCol.remove(keyIn)
                Some((aCol, keyIn))

              case _ =>
                println("Huh?")
                None
            }
          }

          keyOut -> set
        }

        val cpy     = Copy[S, I]
        val pGenNew = cpy(pGen0S)
        cpy.finish()

        if (config.display) {
          val aggrOld = nGenOld.aggregate
          if (aggrOld != null) {
            val bOld  = aggrOld.getBounds
            val ptNew = new Point2D.Double(bOld.getCenterX, bOld.getCenterY)
            panel.prepareAndLocate(pGenNew, ptNew)
          }
        }

        nGenOld.removeSelf()

//        panel.nuages.surface match {
//          case fRoot: Nuages.Surface.Folder[I] =>
//            fRoot.peer.remove(pOld)
//
//          case _: Nuages.Surface.Timeline[I] => ???
//        }
        panel.addNewObject(pGenNew)

        mapOut.foreach { case (keyOut, set) =>
          val outNew = pGenNew.outputs.add(keyOut)
          set.foreach { case (aCol, keyIn) =>
            aCol.get(keyIn) match {
              case Some(f: Folder[I]) => f.addLast(outNew)
              case Some(other) =>
                val f = Folder[I]()
                f.addLast(other)
                f.addLast(outNew)
                aCol.put(keyIn, f)
              case None =>
                aCol.put(keyIn, outNew)
            }
          }
        }
      }
    }
  }

  private def createBasicStructure()(implicit tx: S#Tx): Unit = {
    implicit val tx0: InTxn = tx.peer
    implicit val itx: I#Tx  = bridge(tx)

    val pIdx      = refGraphPos()
    val n         = panel.nuages
    val fGen      = n.generators.get
    val fCol      = n.collectors.get
    val grProc    = grProcH()

    val programOpt = for {
      pIn0    <- fGen.$[Proc]("in")
      pGen0S  <- grProc.at(pIdx).flatMap { e =>
        e.value match {
          case p: Proc[S] => Some(p)
          case _ => None
        }
      }
      pOut0 <- fCol.$[Proc]("O-inf" /*"O-all"*/)
    } yield {
//          val g0 = pGen0S.graph.value
//          val pGen0 = Proc[I]()
//          pGen0.graph() = g0
      val cpyS  = Copy[S, I]
      val pGen  = cpyS(pGen0S)
      cpyS.finish()
      val cpyI  = Copy[I, I]
      val pOut  = cpyI(pOut0)
      val pIn   = cpyI(pIn0)
      cpyI.finish()
      pOut.attr.put(Proc.mainIn, pGen.outputs.add(Proc.mainOut))
      pIn .attr.put("$bal-bus", IntObj.newConst(ciBal))

//      panel.createGenerator(pGen0 , Some(pCol0) , new Point(200, 200))
//      panel.createGenerator(pIn0  , None        , new Point(400, 200))

      val surface = surfaceH()
      surface.addLast(pOut)
      surface.addLast(pGen)
      surface.addLast(pIn)

      refGen()  = itx.newHandle(pGen)
      hndOut    = itx.newHandle(pOut)
      hndIn     = itx.newHandle(pIn)

      ()
    }

    if (programOpt.isEmpty) {
      println("! createBasicStructure failed")
    }
  }

  var showLog: Boolean = config.log

  def fullVersion: String = "v0.1.0-SNAPSHOT"

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
