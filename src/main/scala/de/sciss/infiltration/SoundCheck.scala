package de.sciss.infiltration

import de.sciss.lucre.synth.{InMemory, Server, Synth, Txn}
import de.sciss.mellite.Mellite
import de.sciss.synth.proc.AuralSystem
import de.sciss.synth.{SynthGraph, Server => SServer}
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

object SoundCheck {
  case class Config(amp: Double = 0.2, loop: Boolean = false)

  def main(args: Array[String]): Unit = {
    val default = Config()

    object p extends ScallopConf(args) {
      printedName = "SoundCheck"

      val amp: Opt[Double] = opt(default = Some(default.amp),
        descr = s"Amplitude linear (default: ${default.amp})"
      )
      val loop: Opt[Boolean] = toggle(default = Some(default.loop),
        descrYes = "Store bad synth definitions in user home",
      )

      verify()
      val config: Config = Config(
        amp   = amp(),
        loop  = loop(),
      )
    }

    Mellite.initTypes()
    run(p.config)
  }

  def run(config: Config): Unit = {
    val aCfg                = SServer.Config()
    aCfg.deviceName         = Some(Infiltration.deviceName)
    aCfg.inputBusChannels   = 2
    aCfg.outputBusChannels  = 6
    val as = AuralSystem()

    type S = InMemory
    val cursor: S = InMemory()

    cursor.step { implicit tx =>
      as.addClient(new AuralSystem.Client {
        def auralStarted(s: Server)(implicit tx: Txn): Unit = {
          val g = SynthGraph {
            import de.sciss.synth.ugen._
            val pulse = LFPulse.kr(2)
            val ch = Stepper.kr(pulse, lo = 0, hi = 4, reset = Impulse.kr(0))
            val sig = WhiteNoise.ar(pulse * (ch < 4)) * 0.2
            if (!config.loop) FreeSelf.kr(ch sig_== 4)
            Out.ar(ch, sig)
          }

          val syn = Synth.play(g, Some("test"))(target = s)
          syn.onEnd {
            sys.exit()
          }
        }

        def auralStopped()(implicit tx: Txn): Unit = ()
      })
      as.start(aCfg)
    }
  }
}
