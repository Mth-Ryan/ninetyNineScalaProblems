package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*
import scala.annotation.tailrec

object Problem17 extends Problem {
  
  override protected val number: Int = 17
  override protected val description: String = "Split a list into two parts; the length of the first part is given"

  def split[A](list: List[A], at: Int): (List[A], List[A]) = {
    @tailrec
    def aux(acc: List[A], count: Int, list: List[A]): (List[A], List[A]) = list match {
      case Nil    => (Problem5.rev(acc), Nil)
      case h :: t => if count == 1 then (Problem5.rev(h::acc), t) else aux (h :: acc, count - 1, t)
    }
    aux(Nil, at, list)
  }

  override protected def exec(): Unit = {
    val args = List("a","b", "c", "d", "e")
    logApply("split", args, 3)
    logResult(split(args, 3))
  }
}
