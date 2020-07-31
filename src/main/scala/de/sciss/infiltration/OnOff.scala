package de.sciss.infiltration

import java.net.InetSocketAddress
import java.util.Locale

import com.pi4j.io.gpio.event.{GpioPinDigitalStateChangeEvent, GpioPinListenerDigital}
import com.pi4j.io.gpio.{GpioFactory, Pin, PinPullResistance, RaspiPin}
import de.sciss.infiltration.Network.OscMute
import de.sciss.osc
import org.rogach.scallop.{ArgType, ScallopConf, ValueConverter, ScallopOption => Opt}

import scala.util.control.NonFatal

/*
  brown : +3.3V
  red   : GPIO 18 -- black button
  orange: GPIO 17 -- red button

 */
object OnOff {
  case class Config(
                    pinRed   : Pin = RaspiPin.GPIO_00,
                    pinBlack : Pin = RaspiPin.GPIO_01,
                    pull: PinPullResistance = PinPullResistance.PULL_DOWN,
                    log : Boolean = false,
                    ownSocket: Option[InetSocketAddress] = None,
                    keepAlive : Boolean = false,
                   )

  def main(args: Array[String]): Unit = {
    object p extends ScallopConf(args) {
      printedName = Infiltration.nameAndVersion

      private val default = Config()

      val pinRed: Opt[Int] = opt("pin-red", default = Some(default.pinRed.getAddress),
        descr = s"GPIO pin for red button (default: ${default.pinRed.getAddress})"
      )
      val pinBlack: Opt[Int] = opt("pin-black", default = Some(default.pinBlack.getAddress),
        descr = s"GPIO pin for black button (default: ${default.pinBlack.getAddress})"
      )
      val pull: Opt[String] = opt("pull", default = Some(default.pull.toString),
        descr = s"Resistor pull mode, one of 'UP', 'DOWN, 'OFF' (default ${default.pull})"
      )
      val log: Opt[Boolean] = toggle(default = Some(false),
        descrYes = "Print logging"
      )
      val keepAlive: Opt[Boolean] = toggle("keep-alive", default = Some(false),
        descrYes = "Do not shutdown this node"
      )

      private implicit object InetSocketAddressConverter extends ValueConverter[InetSocketAddress] {
        def parse(s: List[(String, List[String])]): Either[String, Option[InetSocketAddress]] =
          s match {
            case (_, v :: Nil) :: Nil => Infiltration.parseSocket(v).map(Some(_))
            case Nil                  => Right(None)
            case _                    => Left("provide <host>:<port>")
          }

        val argType: ArgType.V = ArgType.SINGLE
      }
      val ownSocket: Opt[InetSocketAddress] = opt(
        descr = "Own IP address"
      )

      verify()

      private def parsePin(i: Int, default: Pin): Pin = i match {
        case  0 => RaspiPin.GPIO_00
        case  1 => RaspiPin.GPIO_01
        case  2 => RaspiPin.GPIO_02
        case  3 => RaspiPin.GPIO_03
        case  4 => RaspiPin.GPIO_04
        case  5 => RaspiPin.GPIO_05
        case  6 => RaspiPin.GPIO_06
        case  7 => RaspiPin.GPIO_07
        case  8 => RaspiPin.GPIO_08
        case  9 => RaspiPin.GPIO_09
        case 10 => RaspiPin.GPIO_10
        case 11 => RaspiPin.GPIO_11
        case 12 => RaspiPin.GPIO_12
        case 13 => RaspiPin.GPIO_13
        case 14 => RaspiPin.GPIO_14
        case 15 => RaspiPin.GPIO_15
        case 16 => RaspiPin.GPIO_16
        case 17 => RaspiPin.GPIO_17
        case 18 => RaspiPin.GPIO_18
        case 19 => RaspiPin.GPIO_19
        case 20 => RaspiPin.GPIO_20
        case 21 => RaspiPin.GPIO_21
        case 22 => RaspiPin.GPIO_22
        case 25 => RaspiPin.GPIO_25
        case 27 => RaspiPin.GPIO_27
        case 28 => RaspiPin.GPIO_28
        case 29 => RaspiPin.GPIO_29
        case _ =>
          println(s"Illegal pin $i. Fall back to ${default.getAddress}")
          default
      }

      private def parsePull(s: String): PinPullResistance = s.toUpperCase(Locale.US) match {
        case "DOWN" => PinPullResistance.PULL_DOWN
        case "UP"   => PinPullResistance.PULL_UP
        case "OFF"  => PinPullResistance.OFF
        case _ =>
          println(s"Illegal pull $s. Fall back to ${default.pull}")
          default.pull
      }

      val config: Config = Config(
        pinRed    = parsePin(pinRed(), default.pinRed),
        pinBlack  = parsePin(pinBlack(), default.pinBlack),
        pull      = parsePull(pull()),
        log       = log(),
        ownSocket = ownSocket.toOption,
        keepAlive = keepAlive(),
      )
    }

    run(p.config)
  }

  def run(config: Config): Unit = {
    println(s"Button control - ${Infiltration.nameAndVersion}")

    val gpio      = GpioFactory.getInstance
    val butRed    = gpio.provisionDigitalInputPin(config.pinRed   , config.pull)
    val butBlack  = gpio.provisionDigitalInputPin(config.pinBlack , config.pull)

//    butRed  .setShutdownOptions(true)
//    butBlack.setShutdownOptions(true)

    val oscCfg  = osc.UDP.Config()
    config.ownSocket.foreach { addr =>
      oscCfg.localSocketAddress = addr
    }
    val trns = osc.UDP.Transmitter(oscCfg)
    trns.connect()

    val targets: Seq[InetSocketAddress] = Network.allDots.map { dot =>
      val port = if (Network.davidDots.contains(dot)) Network.SensorsPort else Network.ClientPort
      new InetSocketAddress(s"192.168.0.$dot", port)
    }

    def sendOsc(m: osc.Message): Unit =
      targets.foreach { tgt =>
        try {
          trns.send(m, tgt)
        } catch {
          case NonFatal(ex) =>
            println(s"For $tgt:")
            println(ex)
        }
      }

    butRed.addListener(new GpioPinListenerDigital() {
      private var t0 = 0L
      private var t1 = 0L
      private var t2 = 0L

      override def handleGpioPinDigitalStateChangeEvent(event: GpioPinDigitalStateChangeEvent): Unit = {
        if (config.log) println(s"red button: ${event.getState}")
        if (event.getState.isHigh) {
          t0 = t1
          t1 = t2
          t2 = System.currentTimeMillis()
          if ((t2 - t0) < 3000) {
            println("Received shutdown cue.")
            sendOsc(Network.OscShutdown)
            if (!config.keepAlive) {
              Thread.sleep(2000)
              Util.shutdown()
            }
          }
        }
      }
    })

    butBlack.addListener(new GpioPinListenerDigital() {
      private var isMuted = false

      override def handleGpioPinDigitalStateChangeEvent(event: GpioPinDigitalStateChangeEvent): Unit = {
        if (config.log) println(s"black button: ${event.getState}")
        if (event.getState.isHigh) {
          isMuted = !isMuted
          println(s"Received mute state: ${isMuted}")
          sendOsc(OscMute(isMuted))
        }
      }
    })

    while (true) {
//      println(s"RED: ${butRed.getState.getValue}")
      Thread.sleep(500)
    }
  }
}
