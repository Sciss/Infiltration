/*
 *  SoundScene.scala
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

trait SoundScene {
//  var showLog: Boolean

//  def fullVersion: String

  def setMainVolume(amp: Double): Unit

  def start(): Unit

  def stop(): Unit

  def testChannel(ch: Int): Boolean

  def serverInfo(): String

  def sensorUpdate(data: Array[Float]): Unit
}
