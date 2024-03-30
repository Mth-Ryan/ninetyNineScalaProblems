package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*
import scala.annotation.tailrec

object Problem14 extends Problem {

  override protected val number: Int = 14
  override protected val description: String = "Duplicate the elements of a list"

  def duplicate[A](list: List[A]): List[A] = {
    @tailrec
    def aux(acc: List[A], list: List[A]): List[A] = list match {
      case Nil    => acc
      case h :: t => aux(h::(h::acc), t)
    }
    Problem5.rev(aux(Nil, list))
  }

  override protected def exec(): Unit = {
    val args = List("a","b", "c", "d", "e")
    logApply("duplicate", args)
    logResult(duplicate(args))
  }
}
