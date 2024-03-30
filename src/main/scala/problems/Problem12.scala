package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*
import scala.annotation.tailrec

object Problem12 extends Problem {

  override protected val number: Int = 12
  override protected val description: String = "Decode a run-length encoded list"

  def repeat[A](value: A, times: Int): List[A] = {
    @tailrec
    def aux(acc: List[A], curr: Int, times: Int, value: A): List[A] = 
      if curr < times then
         aux(value :: acc, curr + 1, times, value)
      else
         acc

    aux(Nil, 0, times, value)
  }
  
  def decode[A](list: List[Problem11.Rle[A]]): List[A] = {
    @tailrec
    def aux(acc: List[A], list: List[Problem11.Rle[A]]): List[A] = list match
      case Nil    => acc
      case Problem11.Rle.One(v) :: t      => aux(acc ::: List(v), t)
      case Problem11.Rle.Many(n, v) :: t  => aux(acc ::: repeat(v, n), t)

    aux(Nil, list)
  }


  override protected def exec(): Unit = {
    val args: List[Problem11.Rle[String]] = List(
      Problem11.Rle.Many(4, "a"),
      Problem11.Rle.One("b"),
      Problem11.Rle.Many(2, "c"),
      Problem11.Rle.One("d"),
      Problem11.Rle.Many(4, "e"),
    )

    logApply("decode", args)
    logResult(decode(args))
  }
}
