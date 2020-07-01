def mkTestValues(min: Double, max: Double): Vector[Double] = {
  def genPos(n: Int, hi: Double): Vector[Double] = {
    val lo    = math.min(0.05, hi / 10000)
    val n1    = n - 1
    val f     = math.max(1.25, math.pow(hi / lo, 1.0 / n1))
    val vec0  = Vector.tabulate(n)(i => lo * math.pow(f, i))
    vec0.takeWhile(_ + 1.0e-4 < hi) :+ hi
  }

  def genNeg(n: Int, lo: Double): Vector[Double] = {
    val hi    = math.max(-0.05, lo / 10000)
    val n1    = n - 1
    val f     = math.max(1.25, math.pow(lo / hi, 1.0 / n1))
    val vec0  = Vector.tabulate(n)(i => hi * math.pow(f, n1 - i))
    lo +: vec0.dropWhile(_ - 1.0e-4 < lo)
  }
  
  def genBi(n: Int, lo: Double, hi: Double): Vector[Double] = {
    val n1    = n - 1
    val f     = math.max(1.25, math.pow(hi / lo, 1.0 / n1))
    val vec0  = if (lo > 0.0) Vector.tabulate(n)(i => lo * math.pow(f, i))
                else          Vector.tabulate(n)(i => hi * math.pow(f, n1 - i))
    lo +: vec0.dropWhile(_ - 1.0e-4 < lo).takeWhile(_ + 1.0e-4 < hi) :+ hi
  }

  if (min == 0.0) {
    assert (max > 0.0)
    0.0 +: genPos(n = 30, hi = max)

  } else if (max == 0.0) {
    assert (min < 0.0)
    genNeg(n = 30, lo = min) :+ 0.0

  } else if (min < 0.0 && max > 0.0) {  // different signum
    genNeg(n = 15, lo = min) ++ (0.0 +: genPos(n = 15, hi = max))

  } else {  // same signum
    assert (math.signum(min) == math.signum(max))
    genBi(n = 31, lo = min, hi = max)
  }
}

def test(min: Double, max: Double): Unit = {
  val v = mkTestValues(min, max)
  println(s"n = ${v.size}")
  println(v.mkString("  ", "\n  ", ""))
}

def include(in: Vector[Double], value: Double): Vector[Double] = {
  require (in == in.sorted)
  val idx0 = in.indexWhere(_ > value)
  val idx  = if (idx0 < 0) in.size else idx0
  in.patch(idx, value :: Nil, 0)
}

test(0.0, 1000.0)
test(-1000.0, 0.0)
test(0.0, 0.0002)
test(-0.1, 200)
test(1.0, 10.0)
test(-10.0, -1.0)

include(mkTestValues(0.0, 10.0), 4.4)
