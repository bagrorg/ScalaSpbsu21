package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite
import org.spbsu.mkn.scala.MyGenericList.{fromSeq, size, sum}

class MyGenericListTest extends AnyFunSuite {
  class ForTest()
  class ForTestInherit() extends ForTest

  def implements[A](list: MyGenericList[A]) = list match {
    case list: MyGenericList[ForTest] => true
    case _ => false
  }

  test("head") {
    assert(fromSeq(Seq(1,2,3)).head == 1)
    assert(fromSeq(Seq(1)).head == 1)
    assertThrows[UnsupportedOperationException](fromSeq(Seq()).head)
  }

  test("tail") {
    assert(fromSeq(Seq(1,2,3)).tail == fromSeq(Seq(2,3)))
    assert(fromSeq(Seq(1)).tail == MyGenericNil)
  }

  test("drop") {
    assert(fromSeq(Seq(1,2,3)).drop(0) == fromSeq(Seq(1,2,3)))
    assert(fromSeq(Seq(1,2,3)).drop(2) == fromSeq(Seq(3)))
    assert(fromSeq(Seq(1,2,3)).drop(3) == MyGenericNil)
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).drop(10))
  }

  test("take") {
    assert(fromSeq(Seq(1,2,3)).take(0) == MyGenericNil)
    assert(fromSeq(Seq(1,2,3)).take(2) == fromSeq(Seq(1,2)))
    assert(fromSeq(Seq(1,2,3)).take(3) == fromSeq(Seq(1,2,3)))
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).take(10))
  }

  test("map") {
    assert(MyGenericNil.map((x: Int) => x * 2) == MyGenericNil)
    assert(fromSeq(Seq(1,2,3)).map(_ * 2) == fromSeq(Seq(2,4,6)))
    assert(fromSeq(Seq(1,2,3)).map(identity) == fromSeq(Seq(1,2,3)))
  }

  test("size") {
    assert(size(MyGenericNil) == 0)
    assert(size(fromSeq(Seq(1,2,3))) == 3)
  }

  test("sum") {
    //assertThrows[UnsupportedOperationException](sum(MyGenericNil))
    assert(sum(fromSeq(Seq(1,2,3))) == 6)
    assert(sum(fromSeq(Seq(1))) == 1)
  }

  test(testName = "additionalTest") {
    assert(fromSeq(Seq('a','b','c')).map((c: Char) => c.toString) == fromSeq(Seq("a","b","c")))
    assert(fromSeq(Seq(1,2,3)).drop(2).map((x: Int) => x.toString) == fromSeq(Seq("3")))
    assert(sum(fromSeq(Seq(1,1,1,1)).map(_ + 0.01)) == 4.04)
    assert(implements(new ForTestInherit() :: MyGenericNil))
  }
}
