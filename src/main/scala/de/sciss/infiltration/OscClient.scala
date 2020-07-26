/*
 *  OscNode.scala
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

import java.net.{InetSocketAddress, SocketAddress}

import de.sciss.osc
import de.sciss.osc.UDP

import scala.concurrent.stm.{InTxn, Txn}
import scala.util.Try
import scala.util.control.NonFatal

object OscClient {
  def apply(scene: SoundScene, config: Config, localSocketAddress: InetSocketAddress): OscClient = {
    val c                 = UDP.Config()
    c.codec               = Network.oscCodec
    c.localSocketAddress  = localSocketAddress
//    c.bufferSize          = 32768   // only higher for sending SynthDefs
    println(s"OscClient local socket $localSocketAddress - dot ${config.dot}")
    val tx                = UDP.Transmitter(c)
    val rx                = UDP.Receiver(tx.channel, c)
    new OscClient(config, tx, rx, scene = scene)
  }

  //  private val DummyDoneFun: File => Unit = _ => ()
}
class OscClient(val config      : Config,
                val transmitter : UDP.Transmitter.Undirected,
                val receiver    : UDP.Receiver.Undirected,
                val scene       : SoundScene,
               ) {

  def oscReceived(p: osc.Packet, sender: SocketAddress): Unit = p match {
    case Network.OscSetVolume(amp) =>
      scene.setMainVolume(amp)

    case Network.OscStart() =>
      scene.start()

    case Network.OscStop() =>
      scene.stop()

    case osc.Message("/server-info") =>
      try {
        val info = scene.serverInfo()
        transmitter.send(osc.Message("/done", "server-info", info), sender)
      } catch {
        case NonFatal(ex) =>
          transmitter.send(osc.Message("/fail", "server-info", ex.toString), sender)
      }

    case osc.Message("/test-channel", ch: Int/*, sound: Int*/, _ /*rest*/ @ _*) =>
      try {
        val ok = scene.testChannel(ch)
//        val ok = (sound >= 0) && scene.testSound(ch / 6, tpe = sound, rest = rest)
        transmitter.send(osc.Message("/done", "test-channel", ch, ok), sender)
      } catch {
        case NonFatal(ex) =>
          val msg = Util.formatException(ex)
          transmitter.send(osc.Message("/fail", "test-channel", ch, msg), sender)
      }

    case _ =>
      oscFallback(p, sender)
  }

//  final val timer = new java.util.Timer("video-timer")
//
//  private[this] val queries = Ref(List.empty[Query[_]])
//
//  final def scheduleTxn(delay: Long)(body: InTxn => Unit)(implicit tx: InTxn): Task =
//    Task(timer, delay)(body)
//
//  final def removeQuery[A](q: Query[A])(implicit tx: InTxn): Unit =
//    queries.transform(_.filterNot(_ == /* ===  */ q))
//
//  protected final def addQuery[A](q: Query[A])(implicit tx: InTxn): Unit =
//    queries.transform(q :: _)

  final protected def oscFallback(p: osc.Packet, sender: SocketAddress): Unit = {
    val wasHandled = false
//    atomic { implicit tx =>
//      val qsIn      = queries()
//      val qOpt      = qsIn.find(_.handle(sender, p))
//      val _handled  = qOpt.isDefined
//      // println(s"for $p looked through ${qsIn.size} queries - handled? ${_handled}")
//      qOpt.foreach(removeQuery(_))
//      _handled
//    }

    if (!wasHandled) p match {
      case Network.OscShell(cmd) =>
        println("Executing shell command:")
        println(cmd.mkString(" "))
        import sys.process._
        val result = Try(cmd.!!).toOption.getOrElse("ERROR")
        transmitter.send(osc.Message("/shell_reply", result), sender)

      case Network.OscShutdown =>
        if (config.isLaptop)
          println("(laptop) ignoring /shutdown")
        else
          Util.shutdown()

      case Network.OscReboot =>
        if (config.isLaptop)
          println("(laptop) ignoring /reboot")
        else
          Util.reboot()

      case Network.OscQueryVersion =>
        transmitter.send(Network.OscReplyVersion(Infiltration.fullVersion), sender)

      case osc.Message("/error"        , _ @ _*) =>
      case osc.Message("/inject-abort" , _ @ _*) =>
      case osc.Message("/inject-commit", _ @ _*) =>

      case _ =>
        Console.err.println(s"Ignoring unknown OSC $p")
        val args = p match {
          case m: osc.Message => m.name +: m.args
          case _: osc.Bundle  => "osc.Bundle" :: Nil
        }
        transmitter.send(osc.Message("/error", "unknown packet" +: args: _*), sender)
    }
  }

  final def sendNow(p: osc.Packet, target: SocketAddress): Unit =
    try {
      transmitter.send(p, target)
    } catch {
      case NonFatal(ex) =>
        Console.err.println(s"Failed to send ${p.name} to $target - ${ex.getClass.getSimpleName}")
    }

  final def sendTxn(target: SocketAddress, p: osc.Packet)(implicit tx: InTxn): Unit =
    Txn.afterCommit { _ =>
      sendNow(p, target)
    }

  final def dumpOSC(): Unit = {
    transmitter.dump(filter = Network.oscDumpFilter)
    receiver   .dump(filter = Network.oscDumpFilter)
  }

  def init(): this.type = {
    receiver.action = oscReceived
    if (config.dumpOsc) dumpOSC()
    transmitter .connect()
    receiver    .connect()
    this
  }

//  private[this] val txnCount = Ref(0)
//
//  final def mkTxnId()(implicit tx: InTxn): Long = {
//    val i = txnCount.getAndTransform(_ + 1)
//    (i.toLong * 1000) + dot
//  }
}
