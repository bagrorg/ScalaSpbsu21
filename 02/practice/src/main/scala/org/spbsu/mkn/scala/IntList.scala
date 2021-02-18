package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

sealed trait IntList {
  def head: Int
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList = ???
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq(seq: Seq[Int]): IntList = ???
  def sum(intList: IntList): Int      = ???
  def size(intList: IntList): Int     = ???
  // extra task: implement sum using foldLeft
  // def foldLeft(???)(???): ??? = ???
}
