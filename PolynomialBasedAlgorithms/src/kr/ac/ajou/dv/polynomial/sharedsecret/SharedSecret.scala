package kr.ac.ajou.dv.polynomial.sharedsecret

import scala.util.Random

import kr.ac.ajou.dv.polynomial.Polynomial

object SharedSecret {
  def apply(n: Int, k: Int, q: BigInt) = {
    val r = new Random
    val coeffs = Vector.fill[BigInt](k)(BigInt(128, r).mod(q))
    val poly = Polynomial(coeffs)
    val xValues = Vector.fill[BigInt](n)(BigInt(128, r).mod(q))
    val values = for (x <- xValues) yield Vector(x, poly.calculate(x, q))
    new SharedSecret(poly, values)
  }
}

case class SharedSecret(p: Polynomial, values: Vector[Vector[BigInt]])

object TestingSharedSecret extends App {
  val r = new Random
  val q = BigInt(128, r)
  println("Chosen prime	= " + q)
  val ss = SharedSecret(10, 5, q)
  println("Polynomial	= " + ss.p)
  println("Secret key	= " + ss.p.getConstant)
  println("Values		= " + ss.values)
  val recovered = Lagrange(ss.values.take(5), q).calculate
  println("Recover		= " + recovered)
  println("Secret key	= " + recovered.getConstant)
}
