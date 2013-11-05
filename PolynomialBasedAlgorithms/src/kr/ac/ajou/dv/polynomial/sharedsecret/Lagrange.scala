package kr.ac.ajou.dv.polynomial.sharedsecret

import scala.math.BigInt.int2bigInt

import kr.ac.ajou.dv.polynomial.Polynomial

object Lagrange {
  def apply(values: Vector[Vector[BigInt]], q: BigInt) = {
    new Lagrange(values, q)
  }
}

class Lagrange(values: Vector[Vector[BigInt]], q: BigInt) {
  def makeOneItemPoly(a: BigInt) = {
    Polynomial(-a, 1)
  }

  private def calculateLt(idx: Int) = {
    val x = values(idx)(0)
    val y = values(idx)(1)

    val numerItems = values map {
      case Vector(a, b) if a != x => makeOneItemPoly(a)
      case _                      => Polynomial(1)
    }
    val denomItems = values map {
      case Vector(a, b) if a != x => (x - a) % q
      case _                      => BigInt(1)
    }
    val numer = numerItems.foldLeft(Polynomial(1))((b, a) => b.multiply(a, q))
    val denom = denomItems.foldLeft(BigInt(1))((b, a) => (b * a) % q)
    numer.multiply(Polynomial((y * denom.modInverse(q)).mod(q)), q)
  }

  def calculate = {
    val part = values.zipWithIndex.map { case (x, i) => calculateLt(i) }
    part.foldLeft(Polynomial(0))((b, a) => b.add(a, q))
  }
}

object TestingLagrange extends App { // List(9x^2 + 4x + 5, List(List(6, 2), List(2, 10), List(10, 9), List(5, 3), List(12, 10)))
  val lag = Lagrange(Vector(Vector(6, 2), Vector(2, 10), Vector(10, 9)), 13)
  println(lag.calculate)
}