/*
 *  CopyFolder.scala
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
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{Copy, Folder, Txn}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Durable, Workspace}
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

object CopyFolder {
  final case class Config(in: File, out: File, folder: String)

  def main(args: Array[String]): Unit = {
    object p extends ScallopConf(args) {
      printedName = "CopyFolder"

      val in: Opt[File] = opt(required = true,
        descr = "Existing input workspace to copy from"
      )
      val out: Opt[File] = opt(required = true,
        descr = "New output workspace to copy to"
      )
      val folder: Opt[String] = opt(required = true,
        descr = "Name of folder to copy"
      )

      verify()
      val config: Config = Config(
        in      = in(),
        out     = out(),
        folder  = folder(),
      )
    }

    import p.config
    val wsInDir   = config.in
    val wsOutDir  = config.out

    Parametrize.init()

    type S    = Durable
    val dbIn  = BerkeleyDB.factory(wsInDir, createIfNecessary = false)
    val wsIn  = Workspace.Durable.read(wsInDir, dbIn)
    val (folderInH, numChildren) = wsIn.system.step { implicit tx =>
      val r         = wsIn.root
      val folderIn  = r.$[Folder](config.folder).getOrElse(sys.error(s"No folder '${config.folder}' found"))
      (tx.newHandle(folderIn), folderIn.size)
    }

    val dbOut = BerkeleyDB.factory(wsOutDir, createIfNecessary = true)
    val wsOut = Workspace.Durable.empty(wsOutDir, dbOut)
    val folderOutH = wsOut.system.step { implicit tx =>
      val r         = wsOut.root
      val folderOut = Folder[S]()
      folderOut.name = config.folder
      r.addLast(folderOut)
      tx.newHandle(folderOut)
    }

    for (i <- 0 until numChildren) {
      println(s"Copying ${i+1}/$numChildren")
      Txn.copy[S, S, Unit] { (txIn: S#Tx, txOut: S#Tx) =>
        val cpy       = Copy[S, S](txIn, txOut)
        val fIn       = folderInH ()(txIn )
        val fOut      = folderOutH()(txOut)
        val childIn   = fIn(i)(txIn)
        val childOut  = cpy(childIn)
        cpy.finish()
        fOut.addLast(childOut)(txOut)
      } (wsIn.cursor, wsOut.cursor)
    }

    wsOut .close()
    wsIn  .close()
    sys.exit(0)
  }
}
