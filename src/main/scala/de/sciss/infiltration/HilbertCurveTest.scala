package de.sciss.infiltration

import de.sciss.file.File
import de.sciss.synth.io.AudioFile
import de.sciss.numbers.Implicits._

object HilbertCurveTest {
  def main(args: Array[String]): Unit = {
    run(direction = true, frame = 1238, base = "55d91895")
//    run(direction = false )
//    test()
  }

  def test(): Unit = {
    val n = 4 // second order
    for (d <- 0 until (n*n)) {
      val (x, y) = d2xy(n = n, d = d)
      val dr = xy2d(n = n, x = x, y = y)
      println(s"$d: ($x, $y)") //  -- $dr
      require (dr == d, dr.toString)
    }
  }

  // direction: `true` for 2-to-1-dimensional encoding, `false` for 1-to-2-dimensional decoding
  def run(base: String, frame: Int, direction: Boolean, glitch: Boolean = false): Unit = {
    val nIn   = if (direction) "dir-h" else "hilbert-curve"
    val nOut  = if (direction) "hilbert-curve" else "reconstructed"
    val fIn   = /*if (direction) new File(s"/data/temp/naya-dir-h.aif") else*/ new File(s"/data/temp/trunk_$base-$frame-$nIn.aif")
    val fOut  = new File(s"/data/temp/trunk_$base-$frame-$nOut.aif")
    if (fOut.exists()) {
      println(s"Not overwriting existing file $fOut")
      return
    }

    val afIn  = AudioFile.openRead (fIn)
    val afOut = AudioFile.openWrite(fOut, afIn.spec)

    try {
      val numFr = afIn.numFrames.toInt
      val n     = math.sqrt(numFr).toInt
      assert (n.isPowerOfTwo)
      assert (n * n == numFr)
      val bufIn   = afIn  .buffer(numFr)
      val bufOut  = afOut .buffer(numFr)
      afIn.read(bufIn)

      var ch = 0
      while (ch < afIn.numChannels) {
        val aIn     = bufIn (ch)
        val aOut    = bufOut(ch)

        if (direction) {
          if (glitch) {
            var d = 0
            while (d < numFr) {
              val (x, y) = d2xy(n = n, d = d)
              val i = y * n + x
              aOut(d) = aIn(i)
              d += 1
            }
          } else {
            var i = 0
            while (i < numFr) {
              val x = i % n
              val y = i / n
              val d = xy2d(n = n, x = x, y = y)
              aOut(d) = aIn(i)
              i += 1
            }
          }

        } else {
          if (glitch) {
            var i = 0
            while (i < numFr) {
              val x = i % n
              val y = i / n
              val d = xy2d(n = n, x = x, y = y)
              aOut(i) = aIn(d)
              i += 1
            }
          } else {
            var d = 0
            while (d < numFr) {
              val (x, y) = d2xy(n = n, d = d)
              val i = y * n + x
              aOut(i) = aIn(d)
              d += 1
            }
          }
        }

        ch += 1
      }

      afOut.write(bufOut)

    } finally {
      afIn  .cleanUp()
      afOut .cleanUp()
    }
  }

  // converts (x,y) to d
  def xy2d (n: Int, x: Int, y: Int): Int = {
    var d = 0
    var s = n/2
    var xt = x
    var yt = y
    while (s > 0) {
      val rx = if ((xt & s) > 0) 1 else 0
      val ry = if ((yt & s) > 0) 1 else 0
      d += s * s * ((3 * rx) ^ ry)
      val tup = rot(n = n, x0 = xt, y0 = yt, rx = rx, ry = ry)
      xt = tup._1
      yt = tup._2
      s >>= 1
    }
    d
  }

  // converts d to (x,y)
  def d2xy(n: Int, d: Int): (Int, Int) = {
    var x = 0
    var y = 0
    var s = 1
    var t = d
    while (s < n) {
      val rx = 1 & (t/2)
      val ry = 1 & (t ^ rx)
      val tup = rot(n = s, x0 = x, y0 = y, rx = rx, ry = ry)
      x = tup._1
      y = tup._2
      x += s * rx
      y += s * ry
      t /= 4
      s <<= 1
    }
    (x, y)
  }

  // rotates/flips a quadrant appropriately
  def rot(n: Int, x0: Int, y0: Int, rx: Int, ry: Int): (Int, Int) = {
    var x = x0
    var y = y0
    if (ry == 0) {
      if (rx == 1) {
        val n1 = n - 1
        x = n1 - x
        y = n1 - y
      }

      // swap x and y
      val t  = x
      x = y
      y = t
    }
    (x, y)
  }
}
