package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*

import scala.annotation.tailrec

object Problem1 extends Problem {
  val number: Int = 1
  val description: String = "Write a function last : List[A] -> Option[A] that returns the last element of a list"

  @tailrec
  def last[A](list: List[A]): Option[A] = list match {
    case Nil    => None
    case h::Nil => Some(h)
    case _::t   => last(t)
  }
    

  def exec(): Unit = {
    val args1 = List("a", "b", "c", "d")
    logApply("last", args1)
    logResult(last(args1))

    logApply("last", List())
    logResult(last(List()))
  }
}


