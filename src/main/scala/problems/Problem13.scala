package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*
import scala.annotation.tailrec
import scala.Conversion
import scala.language.implicitConversions

object Problem13 extends Problem {

  override protected val number: Int = 13
  override protected val description: String = "Run-length encoding of a list (direct solution)"

  enum Rle[A] {
    case One(value: A)
    case Many(n: Int, value: A)
  }

  given relConversion[A]: Conversion[(Int, A), Rle[A]] with
    def apply(tuple: (Int, A)): Rle[A] = tuple match {
      case (1, v) => Rle.One(v)
      case (n, v) => Rle.Many(n, v)
    }


  def encode[A](list: List[A]): List[Rle[A]] = {
    @tailrec
    def aux(count: Int, acc: List[Rle[A]], list: List[A]): List[Rle[A]] = list match {
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
