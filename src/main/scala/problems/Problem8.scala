package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*

import scala.annotation.tailrec

object Problem8 extends Problem {
  val number = 8
  val description = "Eliminate consecutive duplicates of list elements"

  def rev[A](list: List[A]): List[A] = {
    @tailrec
    def aux(acc: List[A], list: List[A]): List[A] = list match {
      case Nil  => acc
      case h::t => aux(h::acc, t)
    }
    aux(List(), list)
  }
  
  @tailrec
  def dropWhile[A](list: List[A], condition: A => Boolean): List[A] = list match {
    case list @ (h :: t) => if condition(h) then dropWhile(t, condition) else list
    case Nil             => Nil
  }
  
  def compress[A](list: List[A]): List[A] = {
    @tailrec
    def aux(acc: List[A], list: List[A]): List[A] = list match {
      case h :: t => aux(h :: acc, dropWhile(t, _ == h))
      case Nil    => acc
    }
    rev(aux(List(), list))
  }

  def exec(): Unit = {
    val arg = List("a", "a", "b", "b", "c", "c")
    logApply("compress", arg)
    logResult(compress(arg))
  }
}
