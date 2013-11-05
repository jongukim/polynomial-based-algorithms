package kr.ac.ajou.dv.polynomial.groupkey

object Utils {
  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    println("time: " + "%,.3f".format((System.nanoTime - s) / 1e6) + " ms")
    ret
  }
}
