package se.hardchee.S99

object Challenge06 extends Challenge {
  def isPalindrome[A](list: List[A]): Boolean = list match {
    case Nil => true
    case x :: Nil => true
    case x :: xs if(x == xs.last) => isPalindrome(xs.init)
    case _ => false
  }

  def go = {
    Some(isPalindrome(List(1, 2, 3, 2, 1)))
  }
}
