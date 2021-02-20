package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

sealed trait IntList {
  def head: Int

  def tail: IntList

  def drop(n: Int): IntList

  def take(n: Int): IntList

  def map(f: Int => Int): IntList

  def ::(elem: Int): IntList
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq(seq: Seq[Int]): IntList = seq match {
    case Nil => IntNil
    case _ => seq.head :: (this fromSeq seq.tail)
  }

  def sum(list: IntList): Int = list match {
    case IntNil => undef
    case _ => foldLeft(0)((el: Int, headSum: Int) => el + headSum)(list)
  }

  def size(list: IntList): Int = foldLeft(0)((_: Int, headSize: Int) => headSize + 1)(list)

  // extra task: implement sum using foldLeft
  @tailrec
  def foldLeft(ini: Int)(f: (Int, Int) => Int)(list: IntList): Int = list match {
    case IntNil => ini
    case IntCons(h, t) => foldLeft(f(h, ini))(f)(t)
  }
}

case object IntNil extends IntList {
  override def ::(elem: Int): IntList = IntCons(elem, IntNil)

  override def head: Int = undef

  override def tail: IntList = undef

  override def drop(n: Int): IntList = n match {
    case 0 => IntNil
    case _ => undef
  }

  override def take(n: Int): IntList = n match {
    case 0 => IntNil
    case _ => undef
  }

  override def map(f: Int => Int): IntList = IntNil
}

case class IntCons(head: Int, tail: IntList) extends IntList {
  override def ::(elem: Int): IntList = IntCons(elem, head :: tail)

  override def drop(n: Int): IntList = n match {
    case 0 => this
    case n if n > 0 => tail drop(n - 1)
    case _ => undef
  }

  override def take(n: Int): IntList = n match {
    case 0 => IntNil
    case n if n > 0 => head :: (tail take(n - 1))
    case _ => undef
  }

  override def map(f: Int => Int): IntList = f(head) :: (tail map f)
}