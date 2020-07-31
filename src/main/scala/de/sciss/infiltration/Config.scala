/*
 *  Config.scala
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

case class Config(
                   baseDir              : File          = userHome/"Documents"/"projects"/"Infiltration",
                   dumpOsc              : Boolean       = false,
                   dumpSensors          : Boolean       = false,
                   isLaptop             : Boolean       = false,
                   disableEnergySaving  : Boolean       = true,
                   qjLaunch             : Boolean       = false,
                   ownSocket            : Option[InetSocketAddress] = None,
                   dot                  : Int           = -1,
                   log                  : Boolean       = true, // false,
                   display              : Boolean       = true,
                   highPass             : Int           = 0,
                   sensorNoiseFloor     : Float         = 1.0e-3f,
                   sensorTrigThreshUp   : Float         = 0.25f,
                   sensorTrigThreshDn   : Float         = 0.15f,
                   flipTrigDurSec       : Int           = 5 * 60,
                   forgetDurSec         : Int           = 2 * 60,
                   maxMainGain          : Double        = 2.0,
                   displayWidth         : Int           = 1000,
                   displayHeight        : Int           = 640,
                   micDial              : Double        = 0.85,
                   lowPass              : Int           = 15000, // 0,
                 ) {

}
