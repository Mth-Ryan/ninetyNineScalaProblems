package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*

object Problem3 extends Problem {
  val number = 3
  val description = "Find the K'th element of a list"

  def at[A](idx: Int, list: List[A]): Option[A] = list match {
    case Nil  => None
    case h::t => if idx == 1 then Some(h) else at(idx - 1, t)
  }
    


  def exec(): Unit = {
    val args1 = List("a", "b", "c", "d")
    logApply("at", 3, args1)
    logResult(at(3, args1))

    val args2 = List("a")
    logApply("at", 3, args2)
    logResult(at(3, args2))
  }
}
