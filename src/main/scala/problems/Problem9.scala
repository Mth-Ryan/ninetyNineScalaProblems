package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*

object Problem9 extends Problem {
  val number = 9
  val description = "Pack consecutive duplicates of list elements into sublists"
  
  def pack[A](list: List[A]): List[List[A]] = {
    def aux(current: List[A], acc: List[List[A]], list: List[A]): List[List[A]] = list match {
      case Nil           => Nil
      case List(a)       => (a::current) :: acc
      case a :: (b :: t) => if a == b then aux(a :: current, acc, t) else aux(Nil, (a :: current) :: acc, t)
    }
    Problem5.rev(aux(Nil, Nil, list))
  }

  def exec(): Unit = {
  }
}
