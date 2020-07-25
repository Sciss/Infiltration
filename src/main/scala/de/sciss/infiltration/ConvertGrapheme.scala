package de.sciss.infiltration

import de.sciss.file.File
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{Copy, Folder, Sys, Txn}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.graph.ScanOut
import de.sciss.synth.proc.{Durable, Grapheme, Proc, Workspace}
import de.sciss.synth.{audio, ugen}
import de.sciss.synth.ugen.BinaryOpUGen
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

object ConvertGrapheme {
  final case class Config(in: File, out: File)

  def main(args: Array[String]): Unit = {
    object p extends ScallopConf(args) {
      printedName = "ConvertGrapheme"

      val in: Opt[File] = opt(required = true,
        descr = "Existing input workspace to copy from"
      )
      val out: Opt[File] = opt(required = true,
        descr = "New output workspace to copy to"
      )

      verify()
      val config: Config = Config(
        in  = in(),
        out = out(),
      )
    }

    Parametrize.init()
    run(p.config)
  }

  def run(config: Config): Unit = {
    type S        = Durable
    val dbIn      = BerkeleyDB.factory(config.in, createIfNecessary = false)
    val wsIn      = Workspace.Durable.read(config.in, dbIn)
    val nameCircle = "circle"
    val graphemeInH = wsIn.system.step { implicit tx =>
      val r           = wsIn.root
      val graphemeIn  = r.$[Grapheme](nameCircle).getOrElse(sys.error(s"No grapheme '$nameCircle' found"))
      tx.newHandle(graphemeIn)
    }

    val dbOut = BerkeleyDB.factory(config.out, createIfNecessary = true)
    val wsOut = Workspace.Durable.empty(config.out, dbOut)
    val (graphemeOutH, folderOutH) = wsOut.system.step { implicit tx =>
      val r           = wsOut.root
      val graphemeOut = Grapheme[S]()
      graphemeOut.name = nameCircle
      val folderOut   = Folder[S]()
      folderOut.name  = "proc"
      r.addLast(graphemeOut)
      r.addLast(folderOut)
      (tx.newHandle(graphemeOut), tx.newHandle(folderOut))
    }

    Txn.copy[S, S, Unit] { (txIn, txOut) =>
      val graphemeIn  = graphemeInH ()(txIn)
      val graphemeOut = graphemeOutH()(txOut)
      val folderOut   = folderOutH()(txOut)
      runWith[S, S](graphemeIn, graphemeOut, Some(folderOut))(txIn, txOut)
    } (wsIn.cursor, wsOut.cursor)

    wsIn  .close()
    wsOut .close()
    sys.exit()
  }

  private def runWith[In <: Sys[In], Out <: Sys[Out]](graphemeIn: Grapheme[In],
                                                      graphemeOut: Grapheme.Modifiable[Out],
                                                      folderOut: Option[Folder[Out]],
                                                     )(implicit txIn: In#Tx, txOut: Out#Tx): Unit = {
    val numProcs    = graphemeIn.lastEvent(txIn).get.toInt + 1
    for (pIdx <- 0 until numProcs) {
      val procIn  = graphemeIn.at(pIdx).get.value match {
        case p: Proc[In] => p
        case _ => sys.error("Unexpected object")
      }
      val procOut   = Proc[Out]()
      val nameIn    = procIn.name
      val negCode   = {
        val i = nameIn.indexOf("-") + 1
        val j = nameIn.indexOf("-", i)
        nameIn.substring(i, j)
      }
      procOut.name  = s"n-$negCode"
      val graphIn   = procIn.graph.value

      /*
        val bus             = "out".kr(0.0)
        val attr            = "gain".kr(1.0)
        val in_7            = times * attr
        Out.ar(bus = bus, in = in_7)

       */

      val (sOut, sBus, sTimes, sGain, sSig) = graphIn.sources.collectFirst {
        case out @ ugen.Out(`audio`, bus, bin @ BinaryOpUGen(BinaryOpUGen.Times, sig, gain)) => (out, bus, bin, gain, sig)
      } .getOrElse(sys.error("No Out UGen"))

      val toRemove = Set(sOut, sBus, sTimes, sGain)

//      println("---- SOURCES ----")
//      println(graphIn.sources.mkString("\n"))
//      println("---- CTL ----")
//      println(graphIn.controlProxies.mkString("\n"))

      val graphOut  = graphIn.copy(
        sources = graphIn.sources.filterNot(toRemove.contains) :+ ScanOut(Proc.mainOut, sSig)
      )

      procOut.graph() = graphOut
      procOut.outputs.add(Proc.mainOut)

      val cpy = Copy[In, Out]
      val attrIn  = procIn  .attr
      val attrOut = procOut .attr
      attrIn.iterator.foreach { case (key, valueIn) =>
        if (key.startsWith("p")) {
          val valueOut = cpy(valueIn)
          attrOut.put(key, valueOut)
        }
      }
      cpy.finish()

      graphemeOut.add(pIdx.toLong, procOut)
      folderOut.foreach(_.addLast(procOut))
    }
  }
}
