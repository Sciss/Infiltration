/*
 *  Util.scala
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

import scala.util.Random

object Util {
  def shutdown(): Unit = {
    Thread.sleep(1000) // this allows for sender == receiver to broadcast first
    import sys.process._
    Seq("sudo", "shutdown", "now").run()
  }

  def reboot(): Unit = {
    Thread.sleep(1000) // this allows for sender == receiver to broadcast first
    import sys.process._
    Seq("sudo", "reboot", "now").run()
  }

  private[this] val uniqueSync  = new AnyRef
  private[this] var uniqueID    = 0

  def nextUniqueID(): Int = uniqueSync.synchronized {
    val res = uniqueID
    uniqueID += 1
    res
  }

  def readTextResource(name: String): String = {
    val url = getClass.getResource(name)
    require(url != null)
    val is  = url.openStream()
    val sz  = is.available()
    val arr = new Array[Byte](sz)
    is.read(arr)
    is.close()
    new String(arr, "UTF-8")
  }

  // rng ops

  def expRand(lo: Double, hi: Double)(implicit random: Random): Double =
    lo * math.exp(math.log(hi / lo) * random.nextDouble())

  def rangeRand(lo: Double, hi: Double)(implicit random: Random): Double =
    random.nextDouble() * (hi - lo) + lo

  /** `lo` to `hi` (inclusive). */
  def rangeRand(lo: Int, hi: Int)(implicit random: Random): Int = {
    if (lo <= hi) {
      random.nextInt(hi - lo + 1) + lo
    } else {
      random.nextInt(lo - hi + 1) + hi
    }
  }

  /** `0 to (i-1)` or `(0 until i)` (exclusive) */
  def rand(i: Int)(implicit random: Random): Int = random.nextInt(i)

  /** `0` until `d` (exclusive). */
  def rand(d: Double)(implicit random: Random): Double = random.nextDouble() * d

  def coin(w: Double = 0.5)(implicit random: Random): Boolean = random.nextDouble() < w

  def choose[A](seq: Seq[A])(implicit random: Random): A =
    seq(random.nextInt(seq.size))

  def weightedChoose[A](seq: Seq[A])(fun: A => Double)(implicit random: Random): A = {
    val i    = random.nextDouble()
    var sum  = 0.0
    var e: A = null.asInstanceOf[A]
    val it = seq.iterator
    while (it.hasNext) {
      e = it.next()
      sum += fun(e)
      if (sum >= i) return e
    }
    e
  }

  def wordWrap(s: String, margin: Int = 80): String = {
    val sz = s.length
    if (sz <= margin) return s
    var i = 0
    val sb = new StringBuilder
    while (i < sz) {
      val j = s.lastIndexOf(" ", i + margin)
      val found = j > i
      val k = if (found) j else i + margin
      sb.append(s.substring(i, math.min(sz, k)))
      i = if (found) k + 1 else k
      if (i < sz) sb.append('\n')
    }
    sb.toString()
  }

  /** Formats the name of the class of a value
   * by inserting space characters at the 'camel' positions.
   */
  def formatClassName(x: Class[_]): String = {
    val cn0 = x.getName
    val i   = cn0.lastIndexOf('.')
    val cn  = cn0.substring(i + 1)
    val len = cn.length
    val b   = new StringBuilder(len + len/2)
    var j   = 0
    var wasUpper = true
    while (j < len) {
      val c       = cn.charAt(j)
      val isUpper = c.isUpper
      if (!wasUpper && isUpper) b.append(' ')
      b.append(c)
      wasUpper    = isUpper
      j += 1
    }
    b.result()
  }

  def formatException(e: Throwable, margin: Int = 80, stackTraceLines: Int = 10): String = {
    val name    = if (e == null) "Exception" else formatClassName(e.getClass)
    val strBuf  = new StringBuilder(name)
    val message = if (e == null) null else {
      val loc = e.getLocalizedMessage
      if (loc == null) e.getMessage else loc
    }
    if (message != null) {
      strBuf.append(":\n")
      strBuf.append(wordWrap(message, margin = margin))
    }
    if (stackTraceLines > 0) {
      val stackS = e.getStackTrace.iterator.take(stackTraceLines).map("   at " + _).mkString("\n")
      strBuf.append("\n")
      strBuf.append(stackS)
    }
    strBuf.result()
  }
}