package ninetyNineScalaProblems.problems

import ninetyNineScalaProblems.core.*

object Problem6 extends Problem {
  val number = 6
  val description = "Find out whether a list is a palindrome"

  def isPalindrome[A](list: List[A]) = list == Problem5.rev(list)

  def exec(): Unit = {
    val args1 = List("x", "a", "m", "a", "x")
    logApply("isPalindrome", args1)
    logResult(isPalindrome(args1))

    
    val args2 = List("b", "a")
    logApply("isPalindrome", args2)
    logResult(isPalindrome(args2))
  }
}
