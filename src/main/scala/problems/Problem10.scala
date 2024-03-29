package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*
import scala.annotation.tailrec

object Problem10 extends Problem {

  override protected val number: Int = 10
  override protected val description: String = "Run-length encoding of a list."

  def encode[A](list: List[A]): List[(Int, A)] = {
    @tailrec
    def aux(count: Int, acc: List[(Int, A)], list: List[A]): List[(Int, A)] = list match {
      case Nil => Nil
      case List(a) => (count + 1, a) :: acc
      case a :: (t @ (b :: _)) => if a == b then aux(count + 1, acc, t) else aux(0, (count + 1, a) :: acc, t)
    }
    Problem5.rev(aux(0, Nil, list))
  }

  override protected def exec(): Unit = {
    val args = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
    logApply("encode", args)
    logResult(encode(args))
  }
}
