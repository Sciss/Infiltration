/*
 *  Sensors.scala
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

import java.net.{InetAddress, InetSocketAddress, SocketAddress}

import de.sciss.osc
import de.sciss.osc.UDP

object Sensors {
  def apply(scene: SoundScene, config: Config): Sensors = {
    val c                 = UDP.Config()
    c.localSocketAddress  = new InetSocketAddress(InetAddress.getLoopbackAddress, Network.SensorsPort)
    val rx                = UDP.Receiver(c)
    new Sensors(config, rx, scene = scene)
  }
}
class Sensors(
               val config      : Config,
               val receiver    : UDP.Receiver.Undirected,
               val scene       : SoundScene,
             ) {

  def oscReceived(p: osc.Packet, sender: SocketAddress): Unit = p match {
    case Network.OscSensors(data) =>
      scene.sensorUpdate(data)

    case _ =>
      Console.err.println(s"Ignoring unknown sensor OSC $p")
  }

  final def dumpOSC(): Unit = {
    receiver.dump(filter = Network.oscDumpFilter)
  }

  def init(): this.type = {
    receiver.action = oscReceived
    if (config.dumpSensors) dumpOSC()
    receiver.connect()
    this
  }
}
