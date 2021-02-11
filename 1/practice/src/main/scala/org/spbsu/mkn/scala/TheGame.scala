package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult

  case class Correct(numTries: Int) extends GuessResult

  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException

  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int): String = Random.alphanumeric.distinct.take(length).foldLeft("")((x, y) => x + y)



  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (userInput.length != secret.length) throw new WrongNumberLengthException(secret.length, userInput.length)
    if (userInput.distinct.length != userInput.length) throw new RepeatingDigitsException

    if (userInput.equals(secret)) Correct(numTries)
    else {
      var bulls = 0
      var cows = 0

      secret.zip(userInput).foreach(tup => if (tup._1 == tup._2) bulls += 1)
      secret.zipWithIndex.foreach(tupSecret => userInput.zipWithIndex.foreach(tupUserIndex => if (tupSecret._1 == tupUserIndex._1 && tupSecret._2 != tupUserIndex._2) cows += 1))

      Incorrect(bulls, cows)
    }
  }

  def main(args: Array[String]): Unit = {
    var numTries = 1
    var isCorrect = false

    println("Write your length")
    val len = readLine().toInt

    val secret = generateNumberString(len)
    println("String is made, start guessing")
    var guess = ""

    while (!isCorrect) {
      guess = readLine()

      validate(secret, guess, numTries) match {
        case Correct(x) => println(s"Right! Your number of tries - ${x}"); isCorrect = true
        case Incorrect(x, y) => println(s"Bulls - ${x}, cows - ${y}"); numTries += 1
      }

    }

  }
}