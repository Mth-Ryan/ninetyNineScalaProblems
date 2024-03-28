package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*

object Problem5 extends Problem {
  val number = 5
  val description = "Reverse a list"

  def rev[A](list: List[A]): List[A] = {
    def aux(acc: List[A], list: List[A]): List[A] = list match {
      case Nil  => acc
      case h::t => aux(h::acc, t)
    }
    aux(List(), list)
  }

  def exec(): Unit = {
    val args1 = List("a", "b", "c")
    logApply("rev", args1)
    logResult(rev(args1))
  }
}
