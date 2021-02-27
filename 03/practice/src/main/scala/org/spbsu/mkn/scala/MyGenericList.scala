package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

import scala.annotation.tailrec

sealed trait MyGenericList[+A] {
  def head: A

  def tail: MyGenericList[A]

  def drop(n: Int): MyGenericList[A]

  def take(n: Int): MyGenericList[A]

  def map[B](f: A => B): MyGenericList[B]

  def ::[B >: A] (elem: B): MyGenericList[B]
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[A](seq: Seq[A]): MyGenericList[A] = seq match {
    case Nil => MyGenericNil
    case _ => seq.head :: (this fromSeq seq.tail)
  }

  def sum[A : Numeric](list: MyGenericList[A]): A = list match {
    case MyGenericNil => undef
    case _ => foldLeft(implicitly[Numeric[A]].zero)((el: A, headSum: A) => implicitly[Numeric[A]].plus(el, headSum))(list)
  }

  def size[A](list: MyGenericList[A]): Int = foldLeft(0)((_: A, headSize: Int) => headSize + 1)(list)

  // extra task: implement sum using foldLeft
  @tailrec
  def foldLeft[B, A](ini: B)(f: (A, B) => B)(list: MyGenericList[A]): B = list match {
    case MyGenericNil => ini
    case MyGenericCons(h, t) => foldLeft(f(h, ini))(f)(t)
  }
}

case object MyGenericNil extends MyGenericList[Nothing] {
  override def ::[B >: Nothing](elem: B): MyGenericList[B] = MyGenericCons(elem, MyGenericNil)

  override def head: Nothing = undef

  override def tail: MyGenericList[Nothing] = undef

  override def drop(n: Int): MyGenericList[Nothing] = n match {
    case 0 => MyGenericNil
    case _ => undef
  }

  override def take(n: Int): MyGenericList[Nothing] = n match {
    case 0 => MyGenericNil
    case _ => undef
  }

  override def map[B](f: Nothing => B): MyGenericList[B] = MyGenericNil
}

case class MyGenericCons[+A](head: A, tail: MyGenericList[A]) extends MyGenericList[A] {
  override def ::[B >: A](elem: B): MyGenericList[B] = MyGenericCons(elem, head :: tail)

  override def drop(n: Int): MyGenericList[A] = n match {
    case 0 => this
    case n if n > 0 => tail drop(n - 1)
    case _ => undef
  }

  override def take(n: Int): MyGenericList[A] = n match {
    case 0 => MyGenericNil
    case n if n > 0 => head :: (tail take(n - 1))
    case _ => undef
  }

  override def map[B](f: A => B): MyGenericList[B] = f(head) :: (tail map f)
}