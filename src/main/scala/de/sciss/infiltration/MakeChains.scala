/*
 *  MakeChains.scala
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
import de.sciss.kollflitz.Vec
import de.sciss.lucre.expr.{DoubleVector, IntVector}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{Folder, Sys}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Durable, Workspace}
import de.sciss.tsp.LinKernighan
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

object MakeChains {
  final case class Config(in: File)

  def main(args: Array[String]): Unit = {
    object p extends ScallopConf(args) {
      printedName = "MakeChains"

      val in: Opt[File] = opt(required = true,
        descr = "Existing input workspace to copy from"
      )

      verify()
      val config: Config = Config(
        in      = in(),
      )
    }

    import p.config
    val wsInDir   = config.in

    Parametrize.init()

    type S    = Durable
    val dbIn  = BerkeleyDB.factory(wsInDir, createIfNecessary = false)
    val wsIn  = Workspace.Durable.read(wsInDir, dbIn)
    import wsIn.cursor
    val folderNameIn    = "par"
    val folderNameCorr  = "pairs"
    val folderNameOut   = "lk"
//    val folderNameOut = "chain"
    val (folderInH, folderCorrH, numChildren, numGroups0, folderOutH) = cursor.step { implicit tx =>
      val r           = wsIn.root
      val folderIn    = r.$[Folder](folderNameIn  ).getOrElse(sys.error(s"No folder '$folderNameIn' found"))
      val folderCorr  = r.$[Folder](folderNameCorr).getOrElse(sys.error(s"No folder '$folderNameCorr' found"))
      val folderOut   = r.$[Folder](folderNameOut).getOrElse {
        val f = Folder[S]()
        f.name = folderNameOut
        r.addLast(f)
        f
      }
      (tx.newHandle(folderIn), tx.newHandle(folderCorr), folderIn.size, folderCorr.size, tx.newHandle(folderOut))
    }

    val count = cursor.step { implicit tx =>
      val fIn = folderInH()
      fIn.iterator.foldLeft(0) {
        case (acc, child: Folder[S]) => acc + child.size
        case (acc, _) => acc
      }
    }

    println(s"In ${wsInDir.name}, the number of children inside $numChildren sub-folders of '$folderNameIn' is $count")
    val groupSize0  = math.sqrt(count)
    val numGroups   = math.round(count / groupSize0).toInt
    val groupSize   = count /*.toDouble*/ / numGroups
    println(s"The grouping size is $groupSize; numGroups = $numGroups; corr folder has $numGroups0 elements")
    require (numGroups == numGroups0)

    var startIdx = 0
    for (groupIdx <- 0 until numGroups) {
      val (numVertices0, edges) = wsIn.cursor.step { implicit tx =>
        readGraph[S](folderCorrH(), group = groupIdx)
      }
      println(s"numVertices = $numVertices0")  // 176
      val m0: Map[Int, Map[Int, Double]] = mkEdgeCostMap(edges)
      val m = m0.filterNot { case (_, values) =>
        values.exists(_._2.isNaN)
      }
      val keysF       = m.keys.toVector.sorted
      val numVertices = keysF.size
      println(s"$numVertices of $numVertices0 vertices are ok.")
//      println(keysF)

      val cost = Array.ofDim[Double](numVertices, numVertices)
      for (vi <- 0 until numVertices) {
        val keyI = keysF(vi)
        for (vj <- (vi + 1) until numVertices) {
          val keyJ      = keysF(vj)
          val valuesOpt = m.get(keyI)
          val values    = valuesOpt.getOrElse(sys.error(s"Not found: $keyI ($vi)"))
          val c         = values(keyJ)
          cost(vi)(vj)  = c
          cost(vj)(vi)  = c
        }
      }
      // randomization does not improve tour
      val tour0 = (0 until numVertices).toArray // 58.243789
      //    val tour0 = util.Random.shuffle((0 until numVertices).toVector).toArray
      val lk    = LinKernighan(cost, tour0)
      println(s"Original cost: ${lk.tourCost}")
      val t0    = System.currentTimeMillis()
      lk.run()
      val t1    = System.currentTimeMillis()
      println(s"Optimization took ${t1-t0}ms.")
      //    val tourOpt = lk.tour
      println(s"Optimized cost: ${lk.tourCost}")
      val tourM = lk.tour.map(keysF)
      println(tourM /*lk.tour*/.mkString(","))
      val tourMOff = tourM.map(_ + startIdx)

      cursor.step { implicit tx =>
        val tourObj   = IntVector.newVar[S](IntVector.newConst(tourMOff))
        tourObj.name  = s"tour-${groupIdx + 1}"
        val folderOut = folderOutH()
        folderOut.addLast(tourObj)
      }

      startIdx += groupSize
    }

    wsIn.close()
    sys.exit(0)
  }

  def mkEdgeCostMap(edges: Seq[WEdge]): Map[Int, Map[Int, Double]] =
    edges.groupBy(_.source).map { case (key, value) =>
      (key, value.groupBy(_.target).map { case (key, value) => assert (value.size == 1); (key, value.head.weight) })
    }

  final case class WEdge(source: Int, target: Int, weight: Double)

  def readGraph[S <: Sys[S]](parent: Folder[S], group: Int)(implicit tx: S#Tx): (Int, Vec[WEdge]) = {
    val b0 = parent.get(group) match {
      case Some(dv: DoubleVector[S])  => dv.value
      case _                          => sys.error("Did not find vector")
    }

    val numEdges    = b0.size // afIn.numFrames.toInt
    val numVertices = (0.5 + math.sqrt(0.25 + numEdges * 2)).round.toInt
    var i = 0
    val edges = for (a <- 0 until (numVertices - 1); b <- (a + 1) until numVertices) yield {
//      val w = 1.0 - b0(i) // high correlation = low cost
      val w = 1.0 - math.sqrt(b0(i)) // high correlation = low cost
      i += 1
      WEdge(a, b, w)
    }
    (numVertices, edges)
  }

}
