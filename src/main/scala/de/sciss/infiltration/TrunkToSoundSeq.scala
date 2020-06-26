/*
 *  TrunkToSoundSeq.scala
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
import de.sciss.fscape.GE
import de.sciss.fscape.lucre.FScape
import de.sciss.infiltration.BuilderUtil._
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.{Durable, SoundProcesses, Widget, Workspace}

object TrunkToSoundSeq {
  final val DEFAULT_VERSION = 1

  case class Config(wsFile: File)

  def main(args: Array[String]): Unit = {
    val projectDir    = file("/data/projects/Infiltration")
    val wsDir         = projectDir / "workspaces"
    val wsFile        = wsDir / "TrunkToSoundSeq.mllt"
    implicit val cf: Config = Config(
      wsFile = wsFile,
    )
    run()
  }

  def run()(implicit config: Config): Unit = {
    SoundProcesses.init()
    FScape        .init()
    Widget        .init()

    type S = Durable

    val store = BerkeleyDB.factory(config.wsFile, createIfNecessary = true)
    implicit val ws: Workspace[S] = Workspace.Durable.empty(config.wsFile, store)
    ws.cursor.
      step { implicit tx =>
        apply[S]()
      }
    ws.close()
    sys.exit()
  }

  def apply[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): Unit = {
    val r     = workspace.root
    val fAux  = mkFolder(r, "aux")
    val fsc   = mkObj[S, FScape](fAux, "TrunkToSound" , DEFAULT_VERSION)(mkFScTrunkToSound[S]())
    /*val gg  = */ mkObj[S, Widget](r, "TrunkToSoundSeq", DEFAULT_VERSION)(mkWidget[S](fsc))
  }

  def mkWidget[S <: Sys[S]](fscRun: FScape[S])
                           (implicit tx: S#Tx): Widget[S] = {
    val c = Widget[S]()
    c.attr.put("run", fscRun)

    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._
    import de.sciss.lucre.swing.graph._
    import de.sciss.synth.proc.MacroImplicits._

    c.setGraph {
      // version: 25-Jun-2020

      val r = Runner("run")
      val m = r.messages
      m.changed.filter(m.nonEmpty) ---> PrintLn(m.mkString("\n"))

      val ggIn = ImageFileIn()
      ggIn.value <--> Artifact("in-temp")
      ggIn.formatVisible = false

      val ggOut = AudioFileOut()
      ggOut.value <--> Artifact("out-temp")
      ggOut.fileType <--> "run:out-type".attr(0)
      ggOut.sampleFormat <--> "run:out-format".attr(1)
      ggOut.sampleRate <--> "run:sample-rate".attr(44100.0)
      ggOut.sampleRateVisible = true

      val ggGain = DoubleField()
      ggGain.unit = "dB"
      ggGain.min  = -180.0
      ggGain.max  = +180.0
      ggGain.value <--> "run:gain-db".attr(0.0)

      val ggGainType = ComboBox(
        List("Normalized", "Immediate")
      )
      ggGainType.index <--> "run:gain-type".attr(0)

      val ggStartFactor = DoubleField()
      ggStartFactor.min  = 0.001
      ggStartFactor.max  = 1000.0
      ggStartFactor.decimals  = 5
      ggStartFactor.step = 0.001
      ggStartFactor.value <--> "run:start-factor".attr(0.29080)

      val ggEndFactor = DoubleField()
      ggEndFactor.min  = 0.001
      ggEndFactor.max  = 1000.0
      ggEndFactor.decimals  = 5
      ggEndFactor.step = 0.001
      ggEndFactor.value <--> "run:end-factor".attr(8.0)

      val ggSlewLimit1 = DoubleField()
      ggSlewLimit1.min  = 0.0
      ggSlewLimit1.max  = 1.0
      ggSlewLimit1.decimals  = 5
      ggSlewLimit1.step = 0.001
      ggSlewLimit1.value <--> "run:slew-limit1".attr(0.001)

      val ggSlewLimit2 = DoubleField()
      ggSlewLimit2.min  = 0.0
      ggSlewLimit2.max  = 1.0
      ggSlewLimit2.decimals  = 5
      ggSlewLimit2.step = 0.001
      ggSlewLimit2.value <--> "run:slew-limit2".attr(0.01)

      val ggDecim = IntField()
      ggDecim.min  = 1
      ggDecim.max  = 128
      ggDecim.value <--> "run:decimation".attr(16)

      val ggColor = DoubleField()
      ggColor.min  = 0.0
      ggColor.max  = 0.99
      ggColor.decimals  = 2
      ggColor.step = 0.01
      ggColor.value <--> "run:color".attr(0.93)

      val ggLeakDC = CheckBox()
      ggLeakDC.selected <--> "run:remove-dc".attr(true)

      val ggSeqFirst = IntField()
      ggSeqFirst.min = 0
      ggSeqFirst.value <--> "seq-first".attr(1)
      val seqFirst0: Ex[Int] = ggSeqFirst.value()

      val ggSeqLast = IntField()
      ggSeqLast.min = 0
      ggSeqLast.value <--> "seq-last".attr(10)
      val seqLast0: Ex[Int] = ggSeqLast.value()

      val seqFirst = seqFirst0 min seqLast0
      val seqLast  = seqFirst0 max seqLast0
      val seqSize: Ex[Int] = seqLast - seqFirst + 1

      def mkLabel(text: String) = {
        val l = Label(text)
        l.hAlign = Align.Trailing
        l
      }

      def left(c: Component*): Component = {
        val f = FlowPanel(c: _*)
        f.align = Align.Leading
        f.vGap = 0
        f
      }

      val p = GridPanel(
        mkLabel("Image Input Template:" ), ggIn,
        mkLabel("Sound Output Template:"), ggOut,
        mkLabel("Sequence first index:"), left(ggSeqFirst),
        mkLabel("Sequence last index:"), left(ggSeqLast),
        mkLabel("Gain:"), left(ggGain, ggGainType),
        Label(" "), Label(""),
        mkLabel("Decimation:"), left(ggDecim),
        mkLabel("Spect Tilt Start Factor:"), left(ggStartFactor),
        mkLabel("Spect Tilt End Factor:"), left(ggEndFactor),
        mkLabel("Slew Limit (Spect):"), left(ggSlewLimit1),
        mkLabel("Slew Limit (Time):"), left(ggSlewLimit2),
        mkLabel("Color (0-1):"), left(ggColor),
        mkLabel("Remove DC:"), ggLeakDC,
      )
      p.columns = 2
      p.hGap = 8
      p.compact = true

      val seqCount  = Var[Int](0)
      val running   = Var[Boolean](false)

      val artIn  = Artifact("run:in")
      val artOut = Artifact("run:out")

      val tempIn  : Ex[File] = ggIn .value()
      val tempOut : Ex[File] = ggOut.value()
      val seqNum = seqFirst + seqCount
      val fIn   = tempIn  .replaceName(tempIn .name.format(seqNum))
      val fOut  = tempOut .replaceName(tempOut.name.format(seqNum))

      val actRun = Act(
        PrintLn("Seq: " ++ seqNum.toStr),
        artIn  set fIn,
        artOut set fOut,
        r.run,
      )

      val actStart = Act(
        running set true,
        seqCount set 0,
        actRun,
      )

      val actStop = Act(
        running set false,
        r.stop,
      )

      val actNext = Act(
        seqCount set seqCount + 1,
        If (seqCount < seqSize) Then actRun Else actStop
      )

      r.stopped ---> actStop
      r.failed  ---> Act(
        PrintLn("FAILED:"),
        PrintLn(r.messages.mkString("\n")),
        actStop
      )
      r.done ---> actNext

      val ggRender  = Button(" Render ")
      val ggCancel  = Button(" X ")
      ggCancel.tooltip = "Cancel Rendering"
      val pb        = ProgressBar()
      ggRender.clicked ---> actStart
      ggCancel.clicked ---> actStop
      //val stopped = (r.state sig_== 0) || (r.state sig_== 4)
      ggRender.enabled = !running
      ggCancel.enabled = running
      //pb.value = (r.progress * 100).toInt
      pb.value = (seqCount * 100.0 / seqSize).toInt

      val bot = BorderPanel(
        center  = pb,
        east    = {
          val f = FlowPanel(ggCancel, ggRender)
          f.vGap = 0
          f
        }
      )
      bot.vGap = 0
      val bp = BorderPanel(
        north = p,
        south = bot
      )
      bp.vGap = 8
      bp.border = Border.Empty(8, 8, 0, 4)
      bp
    }

    c
  }

  def any2stringadd(x: Any): Any = ()

  def mkFScTrunkToSound[S <: Sys[S]]()(implicit tx: S#Tx): FScape[S] = {
    val f = FScape[S]()
    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, ImageFileIn => _, _}
    import de.sciss.fscape.lucre.MacroImplicits._
    import de.sciss.fscape.lucre.graph.Ops._
    import de.sciss.fscape.lucre.graph._

    f.setGraph {
      // version: 25-Jun-2020

      def mkIn() = ImageFileIn("in") // AudioFileIn("in")

      val sampleRate  = "sample-rate" .attr(44100.0)
      val gainType    = "gain-type"   .attr(0)
      val gainDb      = "gain-db"     .attr(0.0)
      val gainAmt     = gainDb.dbAmp
      val fileType    = "out-type"    .attr(0)
      val smpFmt      = "out-format"  .attr(1)
      val startFactor = "start-factor".attr(0.29080)
      val endFactor   = "end-factor"  .attr(8.0)
      val limVal1     = "slew-limit1" .attr(0.001)
      val limVal2     = "slew-limit2" .attr(0.01)
      val decimVal    = "decimation"  .attr(16)
      val color       = "color"       .attr(0.93)
      val removeDC    = "remove-dc"   .attr(1)

      //val in1 = mkIn()

      val (w, h, imgIn) = {
        val in0 = mkIn()
        (in0.width, in0.height, in0.out(0))
      }

      val framesIn = w * h

      //framesIn.poll("framesIn ")
      //val framesOut = framesIn

      def mkHilbert(): GE = {
        val rot = imgIn

        // it "sounds nicer" to remove the DC
        // _before_ unwinding the Hilbert curve
        val leak = If (removeDC) Then {
          val coef = 0.995
          Biquad(rot, b0 = +1.0, b1 = -1.0, a1 = -coef)
        } Else {
          rot
        }

        val hlb = HilbertCurve.To2D(n = w, pos = ArithmSeq(length = framesIn))

        val scan = ScanImage(leak,
          width   = w,
          height  = h,
          x       = hlb.x,
          y       = hlb.y,
        )

        scan
      }

      def mkProgress(x: GE, n: GE, label: String): Unit =
        ProgressFrames(x, n, label)

      val minFactor   = startFactor min endFactor
      // geometric mean -- correct?
      // somehow the actual length factor is off :(
      //val meanFactor  = 2.16356 // 1.0 - 4.0
      //val meanFactor  = 1.44236 // 1.0 - 2.0
      //val meanFactor  = 0.721344 // 1.0 - 0.5
      //val meanFactor  = 0.541011 // 1.0 - 0.25
      //val meanFactor  = 0.84138 // 0.25 - 2.0 lin
      //val meanFactor  = 0.73918 // 0.25 - 4.0 exp
      //val meanFactor  = 1.0
      //val meanFactor  = (startFactor * endFactor).sqrt
      val framesMid   = framesIn // (framesIn * meanFactor).roundTo(1).toLong
      //framesOut.poll("framesOut")
      //val factor      = Line(startFactor, endFactor, framesOut + 1)
      val factor      = Line(0.0, 1.0, framesMid).linExp(0.0, 1.0, startFactor, endFactor)

      val in0 = mkHilbert()
      val in1 = BufferDisk(in0)

      val sig00 = Resample(in0, factor = factor,
        minFactor = minFactor,
        //  rollOff, kaiserBeta, zeroCrossings
      )
      //Length(sig00).poll("resampled")

      //val factor2 = framesIn.toDouble / framesOut
      //val sig0 = (sig000 ++ DC(0.0)).take(framesIn)
      //val framesOut = (framesIn * postFactor).roundTo(1).toLong
      val framesOut = (framesIn / decimVal).toLong
      //framesOut.poll("framesOut")

      //val sig000 = Resample(sig00, factor = postFactor)

      def mkSlew(in: GE, limVal: GE): GE = {
        val dif   = Differentiate(in)
        val lim   = dif.clip2(limVal)
        val int   = RunningSum(lim)
        LeakDC(int)
      }

      val slew1  = mkSlew(sig00, limVal = limVal1)
      val decim = ResizeWindow(slew1, decimVal, stop = -decimVal + 1)

      val fftIn = (decim ++ DC(0.0)).take(framesOut)

      val fftPad  = fftIn ++ DC(0.0).take(framesOut)
      val fft0    = Fourier(fftPad zip DC(0.0), size = framesOut << 1, dir = -1)
      val fft     = fft0.complex.real.take(framesOut)

      val slew2   = mkSlew(in1, limVal = limVal2)
      val sigRsmp0 = Resample(slew2, factor = decimVal.reciprocal)
      val sigRsmp = (sigRsmp0 ++ DC(0.0)).take(framesOut)
      val fb      = color * 0.1 + 0.9
      val env     = OnePole(sigRsmp.squared, fb).sqrt

      val sig0    = fft * env

      val sig = If (gainType sig_== 0) Then {
        val sig0Buf   = BufferDisk(sig0)
        val rMax      = RunningMax(Reduce.max(sig0.abs))
        val read      = Frames(rMax)
        mkProgress(read, framesOut, "analyze")
        val maxAmp    = rMax.last
        val div       = maxAmp + (maxAmp sig_== 0.0)
        val gainAmtN  = gainAmt / div
        sig0Buf * gainAmtN

      } Else {
        sig0 * gainAmt
      }

      val written     = AudioFileOut("out", sig, fileType = fileType,
        sampleFormat = smpFmt, sampleRate = sampleRate)
      mkProgress(written, framesOut, "written")

      //written.last.poll("written  ")
    }
    f
  }
}
