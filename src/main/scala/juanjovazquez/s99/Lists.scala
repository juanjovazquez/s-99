package juanjovazquez.s99

import scala.collection.mutable.ListBuffer
import scala.util.Try

object Lists {

  /**
   * P01. Find the last element of a list
   */
  @annotation.tailrec
  def last[A](x: List[A]): A = x match {
    case Nil => throw new NoSuchElementException
    case x :: Nil => x
    case _ :: xs => last(xs)
  }

  def lastViaReduceRight[A](x: List[A]): A =
    Try(x.reduceRight((_, acc) => acc)).getOrElse(throw new NoSuchElementException)

  def lastViaReduceLeft[A](x: List[A]): A =
    Try(x.reduceLeft((_, a) => a)).getOrElse(throw new NoSuchElementException)

  /**
   * P02. Find the last but one element of a list.
   */
  @annotation.tailrec
  def penultimate[A](l: List[A]): A = l match {
    case a :: _ :: Nil => a
    case x :: xs => penultimate(xs)
    case _ => throw new NoSuchElementException
  }

  /**
   * P03. Find the Kth element of a list.
   */
  @annotation.tailrec
  def nth[A](i: Int, l: List[A]): A = (i, l) match {
    case (0, x :: _) => x
    case (n, _ :: xs) => nth(n - 1, xs)
    case (_, Nil) => throw new IndexOutOfBoundsException
  }

  /** Alternative: using scala built-in support */
  def nthBuiltIn[A](i: Int, l: List[A]): A = l(i)

  /** Alternative: a "functional" version relying on hofs */
  def nthFunctional[A](i: Int, l: List[A]): A =
    l.zip(Stream from 0).filter { _._2 == i }.map { _._1 }.headOption.getOrElse {
      throw new IndexOutOfBoundsException(s"$i")
    }

  /**
   * P04. Find the number of elements of a list.
   */
  def length[A](l: List[A]): Int = {
    @annotation.tailrec
    def go(rest: List[A], acc: Int): Int = rest match {
      case Nil => acc
      case _ :: xs => go(xs, acc + 1)
    }
    go(l, 0)
  }

  def lengthBuiltIn[A](l: List[A]): Int = l.length

  def lengthViaFoldLeft[A](l: List[A]): Int =
    l.foldLeft(0)((acc, _) => acc + 1)

  def lengthViaFoldRight[A](l: List[A]): Int =
    l.foldRight(0)((_, acc) => acc + 1)

  /**
   * P05. Reverse a list.
   */
  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: xs => reverse(xs) ::: List(x)
  }

  def reverseBultin[A](l: List[A]): List[A] = l.reverse

  def reverseTailRec[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go(result: List[A], rest: List[A]): List[A] = rest match {
      case Nil => result
      case x :: xs => go(x :: result, xs)
    }
    go(Nil, l)
  }

  def reverseViaFoldLeft[A](l: List[A]): List[A] =
    l.foldLeft(List.empty[A])((acc, a) => a :: acc)

  def reverseViaFoldRight[A](l: List[A]): List[A] =
    l.foldRight(List.empty[A])((a, acc) => acc ::: List(a))

}
