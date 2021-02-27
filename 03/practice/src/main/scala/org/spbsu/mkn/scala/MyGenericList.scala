package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

sealed trait MyGenericList {
  def head: Int
  def tail: MyGenericList
  def drop(n: Int): MyGenericList
  def take(n: Int): MyGenericList
  def map(f: Int => Int): MyGenericList
  def ::(elem: Int): MyGenericList = ???
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq(seq: Seq[Int]): MyGenericList = ???
  def size(intList: MyGenericList): Int     = ???
  // extra task: implement sum using foldLeft
  // def foldLeft(???)(???): ??? = ???
}
