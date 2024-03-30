package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*
import scala.annotation.tailrec

object Problem15 extends Problem {

  override protected val number: Int = 15
  override protected val description: String = "Replicate the elements of a list a given number of times"

  def replicate[A](list: List[A], n: Int): List[A] = {
    @tailrec
    def aux(acc: List[A], list: List[A], n: Int): List[A] = list match {
      case Nil    => acc
      case h :: t => aux(acc ::: Problem12.repeat(h, n), t, n)
    }
    aux(Nil, list, n)
  }

  override protected def exec(): Unit = {
    val args = List("a","b", "c", "d", "e")
    logApply("replicate", args, 3)
    logResult(replicate(args, 3))
  }
}
