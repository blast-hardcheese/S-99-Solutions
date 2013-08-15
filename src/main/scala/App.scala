package se.hardchee.S99

import scala.collection.immutable.ListMap

trait Challenge {
  def go: Option[Any]
}

object App {
  def challenges = ListMap[Int, Challenge](
    1 -> Challenge01
  )

  def main(args: Array[String]) {
    val result = challenges.lastOption.map({ case (key, challenge) =>
      val result = challenge.go
      println("Result from challenge %d: %s".format(key, result))
    })
  }
}
