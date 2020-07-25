/*
 *  Network.scala
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

import java.io.FileOutputStream
import java.net.{InetAddress, InetSocketAddress, SocketAddress}

import de.sciss.file.file
import de.sciss.kollflitz.Vec
import de.sciss.osc

object Network {
  /** Maps MAC addresses to IP addresses */
  final val macToIPMap: Map[String, String] = Map(
    "dc:a6:32:59:9a:92" -> "192.168.0.30",
    "dc:a6:32:50:aa:59" -> "192.168.0.34",
    "dc:a6:32:50:8b:e3" -> "192.168.0.35",
    "dc:a6:32:50:8d:3d" -> "192.168.0.36",
    "dc:a6:32:50:8c:41" -> "192.168.0.37",
    "dc:a6:32:50:8d:f7" -> "192.168.0.38",
    "dc:a6:32:50:6a:db" -> "192.168.0.39",
    "dc:a6:32:50:8d:64" -> "192.168.0.40",
    "dc:a6:32:50:8c:fe" -> "192.168.0.42",
    "dc:a6:32:50:8b:80" -> "192.168.0.43",
    "dc:a6:32:50:7f:60" -> "192.168.0.44",
    "dc:a6:32:50:8b:f3" -> "192.168.0.45",
    "dc:a6:32:50:8c:2c" -> "192.168.0.46",
  )

  final val dotSeqCtl: Vec[Int] = Vector(36, 37, 40, 42, 43, 44, 77)

  final val ClientPort = 57120

  final val mapDotToTrunk: Map[Int, Int] = Map(
    36 -> 11,
    37 -> 12,
    40 -> 13,
    42 -> 14,
    43 -> 15,
    44 -> 18,
    77 -> 18,
  )

  private def mkSocket(dot: Int): InetSocketAddress = {
    val addr = InetAddress.getByAddress(Array(192.toByte, 168.toByte, 0.toByte, dot.toByte))
    new InetSocketAddress(addr, ClientPort)
  }

  final val socketSeqCtl    : Vec[SocketAddress]      = dotSeqCtl   .map(mkSocket)

  final val dotToSocketMap  : Map[Int, SocketAddress] = (dotSeqCtl zip socketSeqCtl).toMap
  final val socketToDotMap  : Map[SocketAddress, Int] = dotToSocketMap.map(_.swap)

  //  final val dotToSeqMap: Map[Int, Int] = soundDotSeq.zipWithIndex.toMap

  def initConfig(config: Config): InetSocketAddress = {
    val res = config.ownSocket.getOrElse {
      val host = Network.thisIP()
      if (!config.isLaptop) {
        Network.compareIP(host)
      }
      new InetSocketAddress(host, Network.ClientPort)
    }
    checkConfig(config)
    res
  }

  def resolveDot(config: Config, localSocketAddress: InetSocketAddress): Int = {
    if (config.dot >= 0) config.dot else {
      val dot0 = Network.socketToDotMap.getOrElse(localSocketAddress, -1)
      val res = if (dot0 >= 0) dot0 else {
        localSocketAddress.getAddress.getAddress.last.toInt
      }
      if (dot0 < 0) println(s"Warning - could not determine 'dot' for host $localSocketAddress - assuming $res")
      res
    }
  }

  def checkConfig(config: Config): Unit = {
    if (config.disableEnergySaving && !config.isLaptop) {
      import sys.process._
      Seq("xset", "s", "off").!
      Seq("xset", "-dpms").!
    }
  }

  def thisIP(): String = {
    import sys.process._
    // cf. https://unix.stackexchange.com/questions/384135/
    val ifConfig    = Seq("ip", "a", "show", "eth0").!!
    val ifConfigPat = "inet "
    val line        = ifConfig.split("\n").map(_.trim).find(_.startsWith(ifConfigPat)).getOrElse("")
    val i0          = line.indexOf(ifConfigPat)
    val i1          = if (i0 < 0) 0 else i0 + ifConfigPat.length
    val i2          = line.indexOf("/", i1)
    if (i0 < 0 || i2 < 0) {
      val local = InetAddress.getLocalHost.getHostAddress
      Console.err.println(s"No assigned IP4 found in eth0! Falling back to $local")
      local
    } else {
      line.substring(i1, i2)
    }
  }

  /** Verifies IP according to `ipMap` and
   * MAC address. If IP doesn't match, tries
   * to edit `/etc/dhcpcd.conf` and reboot.
   * This way, we can clone the Raspberry Pi
   * image, and each machine can configure
   * itself from the identical clone.
   */
  def compareIP(host: String): Unit = {
    import sys.process._
    val macAddress  = Seq("cat", "/sys/class/net/eth0/address").!!.trim
    macToIPMap.get(macAddress).fold[String] {
      Console.err.println(s"Unknown MAC address: $macAddress - not trying to match IP.")
      host
    } { desiredIP =>
      println(s"This computer has MAC address $macAddress and IP $host")
      if (desiredIP != host && host != "127.0.0.1" && host != "127.0.1.1") {
        val confPath = "/etc/dhcpcd.conf"
        println(s"Designated IP is $desiredIP. Updating /etc/dhcpcd.conf...")
        val header = "interface eth0"
        Seq("cp", confPath, s"$confPath.BAK").!
        val src   = scala.io.Source.fromFile(file(confPath))
        val init  = src.getLines().toList.takeWhile(ln => ln.trim() != header).mkString("\n")
        src.close()
        val tail =
          s"""$header
             |static ip_address=$desiredIP/24
             |""".stripMargin
        val contents = s"$init\n$tail"

        val fOut = new FileOutputStream(confPath)
        fOut.write(contents.getBytes("UTF-8"))
        fOut.close()
        println("Rebooting...")
        Seq("sudo", "reboot", "now").!
      }
      host
    }
  }

  final val oscCodec: osc.PacketCodec =
    osc.PacketCodec().doublePrecision().booleans().packetsAsBlobs()

  final val OscQueryVersion: osc.Message =
    osc.Message("/query", "version")

  object OscReplyVersion {
    private[this] val Name = "/info"
    private[this] val Tpe  = "version"

    def apply(s: String): osc.Message = osc.Message(Name, Tpe, s)

    def unapply(p: osc.Packet): Option[String] = p match {
      case osc.Message(Name, Tpe, s: String) => Some(s)
      case _ => None
    }
  }

  object OscSetVolume {
    private[this] val Name = "/set-volume"

    def apply(amp: Float): osc.Message = osc.Message(Name, amp)

    def unapply(p: osc.Packet): Option[Float] = p match {
      case osc.Message(Name, amp: Float) => Some(amp)
      case _ => None
    }
  }

  object OscStart {
    private[this] val Name = "/start"

    def apply(): osc.Message = osc.Message(Name)

    def unapply(p: osc.Packet): Boolean = p match {
      case osc.Message(Name)  => true
      case _                  => false
    }
  }

  object OscStop {
    private[this] val Name = "/stop"

    def apply(): osc.Message = osc.Message(Name)

    def unapply(p: osc.Packet): Boolean = p match {
      case osc.Message(Name)  => true
      case _                  => false
    }
  }

  object OscShell {
    private[this] val Name = "/shell"

    def apply(cmd: Seq[String]): osc.Message = osc.Message(Name, cmd: _*)

    def unapply(p: osc.Packet): Option[Seq[String]] = p match {
      case osc.Message(Name, cmd @ _*) => Some(cmd.map(_.toString))
      case _ => None
    }
  }

  final val OscShutdown : osc.Message = osc.Message("/shutdown" )
  final val OscReboot   : osc.Message = osc.Message("/reboot"   )
  final val OscHeart    : osc.Message = osc.Message("/heart"    )

  final val oscDumpFilter: osc.Dump.Filter = { p =>
    p.encodedSize(oscCodec) < 1024
  }

  final val TimeOutSeconds: Float = 2.0f
  final val TimeOutMillis : Long  = (TimeOutSeconds * 1000).toLong

  final val HeartPeriodSeconds: Float = 15f
  final val DeathPeriodSeconds: Float = HeartPeriodSeconds * 2.5f

  final val DeathPeriodMillis: Long = (DeathPeriodSeconds * 1000).toLong

  final val MaxPathLen: Int = 37 + 33
}