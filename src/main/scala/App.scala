package se.hardchee.S99

import scala.collection.immutable.ListMap

trait Challenge {
  def go: Option[Any]
}

object App {
  // Adding elements backwards here to minimize scm line changes (having to add a comma after every line)
  def challenges = ListMap[Int, Challenge](
    11 -> Challenge11,
    10 -> Challenge10,
    8 -> Challenge08,
    7 -> Challenge07,
    6 -> Challenge06,
    5 -> Challenge05,
    4 -> Challenge04,
    3 -> Challenge03,
    2 -> Challenge02,
    1 -> Challenge01
  )

  def main(args: Array[String]) {
    val result = challenges.headOption.map({ case (key, challenge) =>
      val result = challenge.go
      println("Result from challenge %d: %s".format(key, result))
    })
  }
}
