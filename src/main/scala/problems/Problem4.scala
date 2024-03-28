package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*

object Problem4 extends Problem {
  val number = 4
  val description = "Find the number of elements of a list"

  def length[A](list: List[A]): Int = {
    def aux(length: Int, list: List[A]): Int = list match {
      case Nil  => length
      case _::t => aux(length + 1, t)
    }

    aux(0, list)
  }
    

  def exec(): Unit = {
    val args1 = List("a", "b", "c")
    logApply("length", args1)
    logResult(length(args1))

    val args2 = List()
    logApply("length", args2)
    logResult(length(args2))
  }
}
