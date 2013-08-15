package se.hardchee.S99

import scala.util.Random

object Challenge24 extends Challenge {
  val random = Random

  def lotto(len: Int, m: Int, sofar: List[Int] = List()): List[Int] = {
    val next: Int = random.nextInt.abs % m
    if(len <= 0) sofar
    else if(sofar contains next) lotto(len, m, sofar)
    else lotto(len - 1, m, sofar :+ next)
  }

  def go = {
    Some(lotto(6, 49))
  }
}
