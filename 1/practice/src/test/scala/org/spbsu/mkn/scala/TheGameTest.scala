package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite

class TheGameTest extends AnyFunSuite {

  import TheGame._

  test("number is generated with given length") {
    assert(generateNumberString(0).isEmpty)
    assert(generateNumberString(1).length == 1)
    assert(generateNumberString(10).length == 10)
  }

  test("generated number has all unique digits") {
    assert(generateNumberString(4).toSet.size == 4)
    assert(generateNumberString(7).toSet.size == 7)
    assert(generateNumberString(10).toSet.size == 10)
  }

  test("mismatched guess length is reported") {
    assertThrows[TheGame.WrongNumberLengthException](validate("0", "000"))
    assertThrows[TheGame.WrongNumberLengthException](validate("12345", "0"))
  }

  test("repeating digits are reported") {
    assertThrows[TheGame.RepeatingDigitsException](validate("0000", "0000"))
  }

  test("incorrect guess is reported") {
    assert(validate("0", "1").isInstanceOf[TheGame.Incorrect])
    assert(validate("12345", "ABCDE").isInstanceOf[TheGame.Incorrect])
  }

  test("correct guess validates") {
    assert(validate("0", "0").isInstanceOf[TheGame.Correct])
    assert(validate("12345", "12345").isInstanceOf[TheGame.Correct])
  }

  test("partially correct guess is reported") {
    assert(validate("12345", "1ABCD") == Incorrect(1, 0) )
    assert(validate("12345", "1AB23") == Incorrect(1, 2) )
    assert(validate("12345", "54A21") == Incorrect(0, 4) )
  }
}
