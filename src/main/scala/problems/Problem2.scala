package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*

object Problem2 extends Problem {
  val number = 2
  val description = "Find the last but one (last and penultimate) elements of a list"

  def lastTwo[A](list: List[A]): Option[(A, A)] = list match {
    case List(_) | Nil => None
    case List(a, b)    => Some((a, b))
    case _::t          => lastTwo(t)
  }

  def exec(): Unit = {
    val args1 = List("a", "b", "c", "d")
    logApply("lastTwo", args1)
    logResult(lastTwo(args1))

    val args2 = List("a")
    logApply("lastTwo", args2)
    logResult(lastTwo(args2))
  }
}
