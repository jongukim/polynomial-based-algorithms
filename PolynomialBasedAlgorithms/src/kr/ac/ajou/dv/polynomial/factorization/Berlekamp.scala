package kr.ac.ajou.dv.polynomial.factorization
import kr.ac.ajou.dv.matrix.BigIntMatrix
import kr.ac.ajou.dv.polynomial.Polynomial
import scala.math.BigInt.int2bigInt

case class Berlekamp(f: Polynomial, q: BigInt) {
  def factorize: Vector[Vector[Polynomial]] = {
    val gFactor = f.gcd(f.differentiate(q), q)
    println("GCD(f(x), f'(x)) = " + gFactor)

    var inc: Int = 0
    def xnInc = {
      val x = inc
      inc += q.intValue
      x
    }
    val B = BigIntMatrix(Vector.fill[Vector[BigInt]](f.getDegree)((Polynomial(Vector.fill[BigInt](xnInc)(0) :+ BigInt(1)).divide(f, q))(1).toVector(f.getDegree)), q)
    println("B =\n" + B)
    val B_I = B.minusImat
    println("B - I =\n" + B_I)
    println("(B - I)t =\n" + B_I.transpose)
    val monicFactors = f.getDegree - B_I.rank
    println("# of distinct monic irreducible factors = " + monicFactors)
    val basisNullSpace = B_I.basisNullSpace
    println(basisNullSpace)

    val hx = for (h <- basisNullSpace.toVectors) yield Polynomial(h)
    for ((h, i) <- hx.zipWithIndex) println("h" + (i + 1) + "(x) = " + h)
    println
    var res = Vector[Vector[Polynomial]]()
    for ((h, i) <- hx.zipWithIndex) {
      var item = Vector[Polynomial]()
      if (h.getDegree > 1) {
        var rx = Polynomial(1)
        for (j <- 0 to monicFactors - 1) {
          var gcd = f.gcd(h.subtract(Polynomial(BigInt(j)), q), q)
//          val Vector(quo, rem) = gcd.divide(gFactor, q)
//          if (rem.isZero) gcd = quo
          println("gcd(f(x), h" + (i + 1) + "(x) - " + j + ") = " + gcd)
          item = item :+ gcd
          rx = rx.multiply(gcd, q)
        }
        println("recovered = " + rx)
        println
      }
      res = res :+ item
    }
    res
  }
}

object BerlekampTest extends App {
  val fx = Polynomial(1, 1, 1, 0, 1)
  println("f(x)  = " + fx)
  println(Berlekamp(fx, 2).factorize)

  val gx = Polynomial(1, 1, 0, 1, 1, 0, 0, 1, 1)
  println("g(x)  = " + gx)
  val bk = Berlekamp(gx, 3)
  println(bk.factorize)
  val tx = Polynomial(2, 0, 0, 2, 0, 0, 0, 2)
  println(tx)
  println(Berlekamp(tx, 3).factorize)
}
