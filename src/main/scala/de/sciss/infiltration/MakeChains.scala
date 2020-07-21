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
  final case class Config(in: File, tie: Boolean)

  def main(args: Array[String]): Unit = {
    object p extends ScallopConf(args) {
      printedName = "MakeChains"

      val in: Opt[File] = opt(required = true,
        descr = "Existing input workspace to copy from"
      )
      val tie: Opt[Boolean] = toggle(
        descrYes = "Tie together the sub-chains", default = Some(false),
      )

      verify()
      val config: Config = Config(
        in      = in(),
        tie     = tie(),
      )
    }

    import p.config
    val wsInDir   = config.in

    Parametrize.init()

    type S    = Durable
    val dbIn  = BerkeleyDB.factory(wsInDir, createIfNecessary = false)
    val wsIn  = Workspace.Durable.read(wsInDir, dbIn)
    import wsIn.cursor
    val folderNamePar   = "par"
    val folderNameCorr  = "pairs"
    val folderNameLK    = "lk"
    val folderNameEnds  = "ends"
//    val folderNameOut = "chain"
    val (folderParH, folderCorrH, numChildren, numGroups0, folderLKH, folderEndsH) = cursor.step { implicit tx =>
      val r           = wsIn.root
      val folderPar   = r.$[Folder](folderNamePar  ).getOrElse(sys.error(s"No folder '$folderNamePar' found"))
      val folderCorr  = r.$[Folder](folderNameCorr).getOrElse(sys.error(s"No folder '$folderNameCorr' found"))
      val folderLK    = r.$[Folder](folderNameLK).getOrElse {
        val f = Folder[S]()
        f.name = folderNameLK
        r.addLast(f)
        f
      }
      val folderEnds   = r.$[Folder](folderNameEnds).getOrElse {
        val f = Folder[S]()
        f.name = folderNameEnds
        r.addLast(f)
        f
      }
      (tx.newHandle(folderPar), tx.newHandle(folderCorr), folderPar.size, folderCorr.size,
        tx.newHandle(folderLK), tx.newHandle(folderEnds))
    }

    def runLK(m: Map[Int, Map[Int, Double]]): Vec[Int] = {

//      val keysF       = m.keys.toVector.sorted
      val keysF       = (m.keySet ++ m.values.flatMap(_.keySet).toSet).toVector.sorted
      val numVertices = keysF.size
      println(s"---numVertices $numVertices")
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
      tourM
    }

    if (config.tie) {
      // tours: total indices
      // ends: zero based
      val (tours, ends) = cursor.step { implicit tx =>
        val folderLK = folderLKH()
        val _tours = folderLK.iterator.collect {
          case iv: IntVector[S] => iv.value
        } .toVector

        val folderEnds = folderEndsH()
//        val _ends = folderEnds.headOption match {
//          case Some(dv: DoubleVector[S]) => dv.value
//          case _ => sys.error("Did not find 'ends'")
//        }
        val (_, _ends) = readGraph(folderEnds, 0)

        (_tours, _ends)
      }
      val numTours  = tours.size
      val numEnds   = 2 * numTours // for each tour, first and last element
      // total
      val endKeys   = tours.map(_.head) ++ tours.map(_.last) // tours.flatMap { t => t.head :: t.last :: Nil }
      println(s"endsKeys.size ${endKeys.size}")
      // from total to index
      val endIdxMap = endKeys.zipWithIndex.toMap
      println(s"Tours: $numTours; ${ends.size} = ${ends.size}")
      require (numEnds * (numEnds - 1) / 2 == ends.size)
      // zero based
      val m0 = mkEdgeCostMap(ends)
      val m0KeysSorted = m0.keys.toVector.sorted
      println(s"KEYS: $m0KeysSorted")
      assert (m0KeysSorted.size == endKeys.size - 1) // because the last node does not have a dictionary
      // for each tour, assign minimum cost to (head, last) edges
      val m = tours.foldLeft(m0) { case (mAcc, tour) =>
        val headK = tour.head
        val lastK = tour.last
        val head  = endIdxMap(headK)
        val last  = endIdxMap(lastK)

//        println(s"headK $headK lastK $lastK head $head last $last")

        def patch(in: Map[Int, Map[Int, Double]], v1: Int, v2: Int): Map[Int, Map[Int, Double]] = {
//          in.get(v1).fold(in) { values0 =>
            val values0 = in(v1)
            assert (values0.contains(v2))
            val values1 = values0 + (v2 -> -1.0) // zero cost
            in + (v1 -> values1)
//          }
        }

        val mP1 = patch(mAcc, head, last)
        val mP2 = mP1 // patch(mP1 , last, head)
        mP2
      }

//      println(s"(43, 32): ${m(43)(32)}")

      val tourM = runLK(m)
      tours.foreach { tour =>
        val headK = tour.head
        val lastK = tour.last
        val head  = endIdxMap(headK)
        val last  = endIdxMap(lastK)
        val hi    = tourM.indexOf(head)
        val li    = tourM.indexOf(last)
        require (hi >= 0, s"headK $headK head $head")
        require (li >= 0, s"lastK $lastK last $last")
        if (math.abs(hi - li) != 1) {
          if (!((hi == 0 && li == tourM.size - 1) || (li == 0 && hi == tourM.size - 1))) {
            println(s"Oops. $head ($hi) and $last ($li) are not neighbours.")
          }
        }
      }

      val tourMTot: Vec[Int] = tourM.map(endKeys.apply)
      println(tourMTot.mkString(","))
      println()
//      println("TOURS")
//      tours.foreach(t => println(s"${t.head} ... ${t.last}"))

      val allTour: Vec[Int] = tourMTot.grouped(2).flatMap {
        case Seq(a, b) =>
          println(s"searching for $a, $b")
          val subOpt  = tours.collectFirst {
            case t if t.head == a && t.last == b => t
            case t if t.head == b && t.last == a => t.reverse
          }
          subOpt.getOrElse(sys.error(s"Oh noes! $a, $b"))
      } .toVector

      println(s"Total tour ${allTour.size}:")
      allTour.grouped(20).foreach(tt => println(tt.mkString("", ",", ",")))

//      cursor.step { implicit tx =>
//        val tourObj   = IntVector.newVar[S](IntVector.newConst(tourMOff))
//        tourObj.name  = "end-tour"
//        val folderOut = folderLKH()
//        folderOut.addLast(tourObj)
//      }

    } else { // no 'tie'
      val count = cursor.step { implicit tx =>
        val fIn = folderParH()
        fIn.iterator.foldLeft(0) {
          case (acc, child: Folder[S]) => acc + child.size
          case (acc, _) => acc
        }
      }

      println(s"In ${wsInDir.name}, the number of children inside $numChildren sub-folders of '$folderNamePar' is $count")
      val groupSize0  = math.sqrt(count)
      val numGroups   = math.round(count / groupSize0).toInt
      val groupSize   = count /*.toDouble*/ / numGroups
      println(s"The grouping size is $groupSize; numGroups = $numGroups; corr folder has $numGroups0 elements")
      require (numGroups == numGroups0)

      var startIdx = 0
      for (groupIdx <- 0 until numGroups) {
        val (numVertices0, edges) = wsIn.cursor.step { implicit tx =>
          readGraph[S](folderCorrH(), index = groupIdx)
        }
        println(s"numVertices = $numVertices0")  // 176
        val m0: Map[Int, Map[Int, Double]] = mkEdgeCostMap(edges)
        val m1 = m0.filterNot { case (_, values) =>
          values.exists(_._2.isNaN)
        }
        println(s"${m1.size} of $numVertices0 vertices are ok.")
        val m = m1.map { case (key, values) =>
          val values1 = values.filter { case (target, _) => m1.contains(target) }
          key -> values1
        }
        val tourM    = runLK(m)
        val tourMOff = tourM.map(_ + startIdx)
        cursor.step { implicit tx =>
          val tourObj   = IntVector.newVar[S](IntVector.newConst(tourMOff))
          tourObj.name  = s"tour-${groupIdx + 1}"
          val folderOut = folderLKH()
          folderOut.addLast(tourObj)
        }

        startIdx += groupSize
      }
    }

    wsIn.close()
    sys.exit(0)
  }

  def mkEdgeCostMap(edges: Seq[WEdge]): Map[Int, Map[Int, Double]] =
    edges.groupBy(_.source).map { case (key, value) =>
      (key, value.groupBy(_.target).map { case (key, value) => assert (value.size == 1); (key, value.head.weight) })
    }

  final case class WEdge(source: Int, target: Int, weight: Double)

  def readGraph[S <: Sys[S]](parent: Folder[S], index: Int)(implicit tx: S#Tx): (Int, Vec[WEdge]) = {
    val b0 = parent.get(index) match {
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
