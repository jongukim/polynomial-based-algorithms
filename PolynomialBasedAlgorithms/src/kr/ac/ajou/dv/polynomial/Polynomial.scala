package kr.ac.ajou.dv.polynomial

import scala.math.BigInt.int2bigInt

object Polynomial {
  def apply(cs: BigInt*): Polynomial = new Polynomial(cs.toVector).trim
  def apply(cs: Vector[BigInt]): Polynomial = new Polynomial(cs).trim
  def apply(p: Polynomial): Polynomial = new Polynomial(p.toVector).trim
}

class Polynomial(coeffs: Vector[BigInt]) {
  def getDegree(): Int = coeffs.length - 1

  def addCoefficient(coeff: BigInt) = Polynomial(toVector :+ coeff)

  def addConstant(const: BigInt, q: BigInt = 0) = {
    val xs = coeffs drop 1
    val added = if (q > 0) (coeffs(0) + const) % q else coeffs(0) + const
    Polynomial(added +: xs)
  }

  def add(p: Polynomial, q: BigInt = 0) = {
    val plus = (coeffs, p.toVector).zipped.map((a, b) => if (q > 0) (a + b).mod(q) else (a + b))
    val res = (coeffs.length - p.toVector.length) match {
      case d if d > 0 => plus ++ coeffs.drop(p.toVector.length)
      case d if d < 0 => plus ++ p.toVector.drop(coeffs.length)
      case _          => plus
    }
    Polynomial(res).trim
  }

  def subtract(p: Polynomial, q: BigInt = 0) = {
    val plus = (coeffs, p.toVector).zipped.map((a, b) => if (q > 0) (a - b).mod(q) else (a - b))
    val res = (coeffs.length - p.toVector.length) match {
      case d if d > 0 => plus ++ coeffs.drop(p.toVector.length)
      case d if d < 0 => plus ++ p.toVector.drop(coeffs.length).map(a => if (q > 0) (-a).mod(q) else -a)
      case _          => plus
    }
    Polynomial(res).trim
  }

  def multiply(p: Polynomial, q: BigInt = 0): Polynomial = {
    var mul = Polynomial(0)
    for ((c, i) <- coeffs.zipWithIndex) {
      val m = Vector.fill[BigInt](i)(0) ++ p.toVector.map(a => if (q > 0) (a * c).mod(q) else a * c)
      mul = mul.add(Polynomial(m), q)
    }
    mul.trim
  }

  def divide(p: Polynomial, q: BigInt = 0): Vector[Polynomial] = {
    var div = Polynomial()
    var numer = Polynomial(this)

    for (i <- (this.getDegree - p.getDegree) to 0 by -1) {
      val diff = numer.getDegree - p.getDegree
      if (diff == i) {
        val denom = Vector.fill[BigInt](diff)(0) ++ p.toVector
        val mcNumer = numer.toVector.last
        val mcDenom = denom.last
        val d = if (q > 0) (mcNumer * mcDenom.bigInteger.modInverse(q)).mod(q) else mcNumer / mcDenom
        val mDenom = Polynomial(denom map (a => if (q > 0) (a * d).mod(q) else a * d))
        numer = numer.subtract(mDenom, q)
        div = Polynomial(d +: div.toVector).trim
      }
      else div = Polynomial(BigInt(0) +: div.toVector).trim
    }
    Vector(div.trim, numer.trim)
  }

  def calculate(x: BigInt) = {
    coeffs.zipWithIndex.map {
      case (coeff, idx) => coeff * x.pow(idx)
    }.sum
  }

  def calculate(x: BigInt, q: BigInt) = {
    var accx: BigInt = 1
    var y: BigInt = 0
    for (c <- coeffs) {
      y = (y + accx * c).mod(q)
      accx = (accx * x).mod(q)
    }
    y
  }

  def gcd(p: Polynomial, q: BigInt): Polynomial = {
    var a = this.sanitize(q)
    var b = p.sanitize(q)
    if (a.getDegree > b.getDegree) {
      while (true) {
        val Vector(quo, rem) = a.divide(b, q)
        if (rem.isZero) return Polynomial(b)
        a = b
        b = rem
      }
    }
    Polynomial(0)
  }

  def getConstant = coeffs.head

  def sanitize(q: BigInt) = Polynomial(coeffs.map(_.mod(q))).trim

  def differentiate: Polynomial = differentiate(0)

  def differentiate(q: BigInt): Polynomial = {
    val d = for ((c, i) <- coeffs.zipWithIndex.drop(1)) yield (if (q > 0) (c * i).mod(q) else c * i)
    Polynomial(d).trim
  }

  def isZero: Boolean = {
    if (coeffs.size == 0 || (getDegree() == 0 && coeffs(0) == 0)) return true
    false
  }

  def toVector(): Vector[BigInt] = coeffs

  def toVector(len: Int): Vector[BigInt] = {
    val coeffList = toVector
    val coLen = coeffList.size
    if (coLen > len) return coeffList.take(len)
    else if (coLen < len) return Vector.concat(coeffList, Vector.fill[BigInt](len - coLen)(0))
    coeffList
  }

  def trim = {
    val p = new Polynomial(coeffs.reverse.dropWhile(_ == 0).reverse)
    if (p.toVector.length == 0) new Polynomial(Vector.fill[BigInt](0)(0)) else p
  }

  override def toString: String = {
    val str = new StringBuilder
    for ((c, i) <- coeffs.zipWithIndex.reverse) {
      if (c != 0) {
        if (i != coeffs.length - 1) str.append(if (c.signum < 0) " - " else " + ")
        if (i == 0) str.append(c.abs)
        else if (c != 1) str.append(if (i != coeffs.length - 1) c.abs else c)
        if (i > 0) str += 'x'
        if (i > 1) str.append("^" + i)
      }
      else if (i == 0 && str.length == 0) str.append(c)
    }
    str.toString
  }
}

object TestingPolynomial extends App {
  val p1 = Polynomial(1, 2, 1)
  val p2 = Polynomial(2, 3, 2, 1)
  println("P1          = " + p1)
  println("P2          = " + p2)
  println("P1 + P2     = " + p1.add(p2))
  println("P1 + P2 % 3 = " + p1.add(p2, 3))
  println("P1 - P2     = " + p1.subtract(p2))
  println("P1 - P2 % 3 = " + p1.subtract(p2, 3))
  println("P1 * P2     = " + p1.multiply(p2))
  println("P1 * P2 % 3 = " + p1.multiply(p2, 3))
  println("Trim test   = " + Polynomial(2, 3, 2).subtract(Polynomial(1, 2, 2)))
  val divrem = p2.divide(p1)
  println("P2 / P1     = " + divrem(0) + " [rem: " + divrem(1) + "]")

  val p3 = Polynomial(4, 0, 3, 4)
  val p4 = Polynomial(2, 2)
  println("P3          = " + p3)
  println("P4          = " + p4)
  val dr2 = p3.divide(p4, 5)
  println("P3 / P4     = " + dr2(0) + " [rem: " + dr2(1) + "]")
  println("P3 = P4*d+r = " + p4.multiply(dr2(0), 5).add(dr2(1), 5))

  println("P2'         = " + p2.differentiate)
  println("P2' % 3     = " + p2.differentiate(3))

  val fx = Polynomial(1, 1, 1, 0, 1)
  val dfx = fx.differentiate(2)
  println("f(x)   = " + fx)
  println("f'(x)  = " + dfx)

  println(fx.gcd(fx.differentiate(2), 2))

  val gx = Polynomial(1, 1, 0, 1, 1, 0, 0, 1, 1)
  println("g(x)  = " + gx)
  println(gx.gcd(gx.differentiate(3), 3))
  val Vector(quo, rem) = gx.divide(Polynomial(BigInt(2)), 3)
  println(quo.multiply(Polynomial(BigInt(2)), 3).add(rem, 3))
  println(gx.divide(Polynomial(BigInt(2)), 3))
  println(gx.differentiate(3).divide(Polynomial(BigInt(2)), 3))
}
