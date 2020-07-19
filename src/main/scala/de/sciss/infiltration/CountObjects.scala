/*
 *  CountObjects.scala
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
import de.sciss.lucre.stm.Folder
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Durable, Workspace}
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

object CountObjects {
  final case class Config(in: File, folder: String)

  def main(args: Array[String]): Unit = {
    object p extends ScallopConf(args) {
      printedName = "CountObjects"

      val in: Opt[File] = opt(required = true,
        descr = "Existing input workspace to copy from"
      )
      val folder: Opt[String] = opt(required = true,
        descr = "Name of folder to count"
      )

      verify()
      val config: Config = Config(
        in      = in(),
        folder  = folder(),
      )
    }

    import p.config
    val wsInDir   = config.in

    Parametrize.init()

    type S    = Durable
    val dbIn  = BerkeleyDB.factory(wsInDir, createIfNecessary = false)
    val wsIn  = Workspace.Durable.read(wsInDir, dbIn)
    val (folderInH, numChildren) = wsIn.system.step { implicit tx =>
      val r         = wsIn.root
      val folderIn  = r.$[Folder](config.folder).getOrElse(sys.error(s"No folder '${config.folder}' found"))
      (tx.newHandle(folderIn), folderIn.size)
    }

    val count = wsIn.cursor.step { implicit tx =>
      val fIn = folderInH()
      fIn.iterator.foldLeft(0) {
        case (acc, child: Folder[S]) => acc + child.size
        case (acc, _) => acc
      }
    }

    println(s"In ${wsInDir.name}, the number of children inside $numChildren sub-folders of '${config.folder}' is $count")

    wsIn.close()
    sys.exit(0)
  }
}
