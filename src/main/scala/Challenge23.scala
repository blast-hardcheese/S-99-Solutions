package se.hardchee.S99

import scala.util.Random

object Challenge23 extends Challenge {
  def randomSelect[A](i: Int, list: List[A], out: List[A] = List()): List[A] = {
    if(i > 0) {
      val index = Random.nextInt.abs % list.length
      val (rest, Some(elem)) = Challenge20.removeAt[A](index, list) // TODO: FIXME: Unsafe, behavior undefined if removeAt fails
      randomSelect(i - 1, rest, out :+ elem)
    } else {
      out
    }
  }

  def go = {
    Some(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
  }
}
