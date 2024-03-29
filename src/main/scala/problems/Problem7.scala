package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*

import scala.annotation.tailrec

object Problem7 extends Problem {
  val number = 7
  val description = "Flatten a nested list structure"

  enum Node[A] {
    case One(value: A)
    case Many(list: List[Node[A]])
  }

  def flatten[A](list: List[Node[A]]): List[A] = {
    def aux(acc: List[A], list: List[Node[A]]): List[A] = list match {
      case Nil => acc
      case Node.One(value)::t => aux(acc ::: List(value), t)
      case Node.Many(list)::t => aux(aux(acc, list), t)
    }
    aux(List(), list)
  }

  def exec(): Unit = {
    var args: List[Node[String]] = List(
      Node.One("a"),
      Node.Many(List(
        Node.One("b"),
        Node.Many(List(
          Node.One("c"),
          Node.One("d")
        )),
        Node.One("e")
      ))
    )

    logApply("flatten", args)
    logResult(flatten(args))
  }
}
