//package kr.ac.ajou.dv.polynomial
//
//import java.math.BigInteger
//import java.util.Random
//
//object NewtonAndBruteForceTest extends App {
//  val DEGREES = 10
//  val COEFF_BITSIZE = 16
//  val rnd = new Random
//
//  def getRandomNum(prime: Boolean = false, minValue: BigInt = 1, size: Int = COEFF_BITSIZE) = {
//    var r: BigInt = 0
//    do {
//      r = if (prime) BigInt(BigInteger.probablePrime(size, rnd)) else BigInt(new BigInteger(size, rnd))
//    } while (r < minValue)
//    r
//  }
//
//  val q = getRandomNum(prime = true, minValue = BigInt(2).pow(COEFF_BITSIZE), size = COEFF_BITSIZE + 1)
//  println("mod: " + q)
//
//  val Gk = getRandomNum()
//  println("Gk: " + Gk)
//
//  val answers = List.fill[BigInt](DEGREES)(getRandomNum())
//
//  def kek2poly(kek: BigInt) = {
//    val p = new Polynomial
//    p.addCoefficient(1).addConstant(-kek)
//  }
//
//  print("Polynomial Expansion: ")
//  val P = answers.map(kek2poly).reduce(_.multiply(_, q)).addConstant(Gk, q)
//  println("P  = " + P)
//
//  val derivedGk = P.calculate(answers(0), q)
//  println("Derived Gk = " + derivedGk);
//
//  P.addConstant(-derivedGk, q)
//  println(" P - Gk   = " + P)
//
//  val diffP = P.differentiate(q)
//  println("(P - Gk)' = " + diffP)
//
//  val MAX_TRIES = 100000
//  val TIMES_GUESSING = 100
//
//  def newton(startingValue: BigInt): BigInt = {
//    var root = startingValue
//    var x0 = root - 1
//    var step = 0
//    var divisor: BigInt = 0
//
//    while (x0 != root && step < MAX_TRIES) {
//      x0 = root
//      divisor = diffP.calculate(x0, q)
//      if (divisor == 0) step = MAX_TRIES
//      else {
//        root = (x0 - P.calculate(x0, q) * divisor.modInverse(q)).mod(q)
//        step += 1
//      }
//    }
//    if (step < MAX_TRIES) root else -1
//  }
//
//  println("Newton's Method")
//  val newtonStartingList = List.fill[BigInt](TIMES_GUESSING)(BigInt(BigInteger.probablePrime(COEFF_BITSIZE, rnd))).toSet[BigInt]
//  println(newtonStartingList.size + " starting values.")
//  val newtonResult = Utils.time(newtonStartingList.par.map(newton).seq.toSet[BigInt].filter(_ > 0))
//  println("Result: " + newtonResult.mkString(", "))
//
//  println("Brute Force Attack")
//  var idx = 0
//
//  def bigIntStream = {
//    idx += 1
//    idx
//  }
//
//  val bfStartingList = Stream.continually(bigIntStream)
//  val bfResult = Utils.time(bfStartingList.takeWhile(_ < BigInt(2).pow(COEFF_BITSIZE)).par
//    .map(P.calculate(_, q)).seq.zipWithIndex.filter(_._1 == 0)).map(_._2 + 1)
//
//  println("Result: " + bfResult)
//}
