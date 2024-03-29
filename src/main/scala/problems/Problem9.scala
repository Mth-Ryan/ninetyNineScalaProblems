package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*

import scala.annotation.tailrec

object Problem9 extends Problem {
  val number = 9
  val description = "Pack consecutive duplicates of list elements into sublists"
  
  def rev[A](list: List[A]): List[A] = {
    @tailrec
    def aux(acc: List[A], list: List[A]): List[A] = list match {
      case Nil  => acc
      case h::t => aux(h::acc, t)
    }
    aux(List(), list)
  }

  def pack[A](list: List[A]): List[List[A]] = {
    @tailrec
    def aux(current: List[A], acc: List[List[A]], list: List[A]): List[List[A]] = list match {
      case Nil           => Nil
      case List(a)       => (a::current) :: acc
      case a :: (t @ (b :: _)) => if a == b then aux(a :: current, acc, t) else aux(Nil, (a :: current) :: acc, t)
    }
    rev(aux(Nil, Nil, list))
  }

  def exec(): Unit = {
    val args = List(1, 1, 2, 2, 3, 3)
    logApply("pack", args)
    logResult(pack(args))

  }
}
