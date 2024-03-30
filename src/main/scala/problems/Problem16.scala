package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*
import scala.annotation.tailrec

object Problem16 extends Problem {
  
  override protected val number: Int = 16
  override protected val description: String = "Drop every N'th element from a list."

  def drop[A](list: List[A], i: Int): List[A] = {
    @tailrec
    def aux(acc: List[A], count: Int, list: List[A]): List[A] = list match {
      case Nil    => acc
      case h :: t => if count == 1 then aux(acc, count - 1, t) else aux(h::acc, count - 1, t)
    }
    Problem5.rev(aux(Nil, i, list))
  }

  override protected def exec(): Unit = {
    val args = List("a","b", "c", "d", "e")
    logApply("drop", args, 3)
    logResult(drop(args, 3))
  }
}
