package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*
import scala.annotation.tailrec

object Problem18 extends Problem {

  override protected val number: Int = 18
  override protected val description: String = "Extract a slice from a list"

  def slice[A](list: List[A], l: Int, r: Int): List[A] = {
    @tailrec
    def aux(acc: List[A], count: Int, list: List[A], l: Int, r: Int): List[A] = list match {
      case Nil => acc
      case h :: t => if count >= l && count <= r then aux(h::acc, count+1, t, l, r) else aux(acc, count+1, t, l, r)
    }
    Problem5.rev(aux(Nil, 0, list, l, r))
  }

  override protected def exec(): Unit = {
    val args = List("a","b", "c", "d", "e")
    logApply("slice", args, 1, 2)
    logResult(slice(args, 1, 2))
  }
}
