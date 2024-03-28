package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*

object Problem6 extends Problem {
  val number = 6
  val description = "Find out whether a list is a palindrome"

  def rev[A](list: List[A]): List[A] = {
    def aux(acc: List[A], list: List[A]): List[A] = list match {
      case Nil  => acc
      case h::t => aux(h::acc, t)
    }
    aux(List(), list)
  }

  def isPalindrome[A](list: List[A]) = list == rev(list)

  def exec(): Unit = {
    val args1 = List("x", "a", "m", "a", "x")
    logApply("isPalindrome", args1)
    logResult(isPalindrome(args1))

    
    val args2 = List("b", "a")
    logApply("isPalindrome", args2)
    logResult(isPalindrome(args2))
  }
}
