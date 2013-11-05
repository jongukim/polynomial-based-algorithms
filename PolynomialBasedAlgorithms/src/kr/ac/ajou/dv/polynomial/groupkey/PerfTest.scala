package kr.ac.ajou.dv.polynomial.groupkey

import java.math.BigInteger
import java.util.Random

import scala.math.BigInt.int2bigInt

import kr.ac.ajou.dv.polynomial.Polynomial

object PerfTest extends App {
  val DEGREES = 1000
  val COEFF_BITSIZE = 1024
  val rnd = new Random

  def getRandomNum(prime: Boolean = false, minValue: BigInt = 1, size: Int = COEFF_BITSIZE) = {
    var r: BigInt = 0
    do {
      r = if (prime) BigInt(BigInteger.probablePrime(size, rnd)) else BigInt(new BigInteger(size, rnd))
    } while (r < minValue)
    r
  }

  val q = getRandomNum(prime = true, minValue = BigInt(2).pow(COEFF_BITSIZE), size = COEFF_BITSIZE + 1)
  println("mod: " + q)

  val Gk = getRandomNum()
  println("Gk: " + Gk)

  val answers = List.fill[BigInt](DEGREES)(getRandomNum())

  def kek2poly(kek: BigInt) = Polynomial(-kek, 1)

  print("Polynomial Expansion: ")
  val P = Utils.time(answers.map(kek2poly).reduce(_.multiply(_, q)).addConstant(Gk, q))
  //  println("P  = " + P)

  print("Deriving a Group Key: ")
  val derivedGk = Utils.time(P.calculate(answers(0), q))
  println("Derived Gk = " + derivedGk);

  print("Deriving a Group Key (faster version): ")
  val derivedGk2 = Utils.time(P.calculate(answers(0), q))
  println("Derived Gk = " + derivedGk2);

  /*
  encryption
   */
  //  val keyGen = KeyGenerator.getInstance("AES")
  //  keyGen.init(128)
  //  val encKeys = List.fill[SecretKey](DEGREES)(keyGen.generateKey())
  //  val cipher = Cipher.getInstance("AES")
  //  val GkBytes = Gk.toByteArray
  //
  //  def encrypt(key: SecretKey) = {
  //    cipher.init(Cipher.ENCRYPT_MODE, key)
  //    cipher.doFinal(GkBytes)
  //  }
  //
  //  def decryption(key: SecretKey, ciphertext: Array[Byte]) = {
  //    cipher.init(Cipher.DECRYPT_MODE, key)
  //    cipher.doFinal(ciphertext)
  //  }
  //
  //  print("Encryption (n times): ")
  //  val encryptedList = Utils.time(encKeys.map(encrypt))
  //
  //  print("Decryption (1 times): ")
  //  val decryptedGk = Utils.time(BigInt(decryption(encKeys(0), encryptedList(0))))
  //  println("Decrypted Gk = " + decryptedGk)
}
