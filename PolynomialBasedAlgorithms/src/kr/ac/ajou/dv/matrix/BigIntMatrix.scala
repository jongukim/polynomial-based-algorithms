package kr.ac.ajou.dv.matrix

import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt.int2bigInt
import scala.math.BigInt.javaBigInteger2bigInt

import FFAO.FFAO

case class BigIntMatrix(mat: Vector[Vector[BigInt]], q: BigInt) {

  def minusImat: BigIntMatrix = {
    val updated = for ((xs, idx) <- mat.zipWithIndex; val up = xs.updated(idx, (xs(idx) - 1).mod(q))) yield up
    BigIntMatrix(updated, q)
  }

  def rowSwap(r1: Int, r2: Int) = {
    val updated = for (
      (r, i) <- mat.zipWithIndex;
      val u = i match {
        case `r1` => mat(r2)
        case `r2` => mat(r1)
        case _    => mat(i)
      }
    ) yield u
    BigIntMatrix(updated, q)
  }

  def rowCal(dst: Int, src: Int, what: FFAO) = {
    val updated = for (
      (r, i) <- mat.zipWithIndex;
      val u = i match {
        case `dst` => mat(dst).zip(mat(src)).map {
          case (a, b) => what match {
            case FFAO.Plus  => (a + b).mod(q)
            case FFAO.Minus => (a - b).mod(q)
          }
        }
        case _ => mat(i)
      }
    ) yield u
    BigIntMatrix(updated, q)
  }

  def updateCal(row: Int, factor: BigInt, what: FFAO) = {
    val updated = for (
      (r, i) <- mat.zipWithIndex;
      val u = i match {
        case `row` => mat(row).map {
          case a => what match {
            case FFAO.Multiply => (a * factor).mod(q)
            case FFAO.Divide   => (a * factor.bigInteger.modInverse(q)).mod(q)
          }
        }
        case _ => mat(i)
      }
    ) yield u
    BigIntMatrix(updated, q)
  }

  def rows = mat.length

  def cols = mat.head.length

  private def rowWeight(row: Vector[BigInt]): Int = row.dropWhile(_ == 0).length

  def getNoneZeroPos(r: Int) = mat(r).indexWhere(_ > 0)

  private def sortEcheonForm = {
    val updated = for (
      r <- mat;
      val head = r.indexWhere(_ > 0);
      val u = if (head < 0) r else {
        val modinverse = r(head).bigInteger.modInverse(q)
        r.map(a => (a * modinverse).mod(q))
      }
    ) yield u
    BigIntMatrix(updated.sortBy(x => rowWeight(x)).reverse, q)
  }

  def isRowEcheonForm: Boolean = {
    for (i <- 1 to rows - 1) {
      val curPos = getNoneZeroPos(i)
      val prevPos = getNoneZeroPos(i - 1)
      if (curPos <= prevPos) return false
    }
    true
  }

  def reducedRowEcheonForm = {
    var r = 1
    var re = sortEcheonForm
    var cur: Int = 1
    var prev: Int = 0
    while (r < re.rows && cur >= 0 && cur >= prev) {
      cur = re.getNoneZeroPos(r)
      prev = re.getNoneZeroPos(r - 1)
      //      println(r, cur, prev)
      if (cur == prev) {
        re = re.rowCal(r, r - 1, FFAO.Minus).sortEcheonForm
        r = 1
      }
      else r = r + 1
      //      println(re)
    }
    re
  }

  def rank = reducedRowEcheonForm.mat.filterNot(p => p.filterNot(_ == 0).length == 0).length

  def transpose = {
    val b = new ArrayBuffer[Vector[BigInt]]
    var y = this.mat
    while (!y.isEmpty) {
      b += y.map(a => a.head)
      y = y.map(_.tail).filterNot(_.isEmpty)
    }
    BigIntMatrix(b.toVector, q)
  }

  def rowAugmented = {
    val updated = for (
      i <- 1 to rows + cols;
      val r = i match {
        case j if j <= rows => mat(j - 1)
        case j if j > rows  => Vector.fill[BigInt](j - rows - 1)(0) ++ Vector[BigInt](1) ++ Vector.fill[BigInt](rows + cols - j)(0)
      }
    ) yield r
    val res: Vector[Vector[BigInt]] = updated.toVector
    BigIntMatrix(res, q)
  }

  def basisNullSpace = {
    val basis = this.transpose.rowAugmented.transpose.reducedRowEcheonForm
    println(basis)
    BigIntMatrix(basis.mat.drop(this.rank).map(_.drop(this.rows)), q)
  }

  def toVectors: Vector[Vector[BigInt]] = mat

  override def toString: String = {
    val maxDigits = mat.flatten.max.toString.length + 1
    val formatStr = "%" + maxDigits + "s"

    val sb = StringBuilder.newBuilder
    val newLine = sys.props("line.separator")

    for (row <- mat) {
      for (col <- row) {
        sb.append(String.format(formatStr, col))
      }
      sb.append(newLine)
    }
    sb.toString
  }
}

object BigIntMatrixTest extends App {
  val A = BigIntMatrix(Vector(Vector(1, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 1, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 1, 0), Vector(1, 0, 2, 1, 0, 2, 0, 1), Vector(0, 1, 0, 0, 1, 2, 0, 0), Vector(1, 1, 0, 1, 2, 0, 0, 2), Vector(1, 0, 0, 0, 1, 0, 2, 0), Vector(2, 0, 1, 0, 0, 1, 0, 0)), 3)

  println(A.minusImat)
  println(A.minusImat.transpose.rowAugmented.transpose.reducedRowEcheonForm)
  println(A.minusImat.basisNullSpace)

}