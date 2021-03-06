package se.hardchee.S99

import scala.collection.immutable.ListMap

trait Challenge {
  def go: Option[Any]
}

object App {
  // Adding elements backwards here to minimize scm line changes (having to add a comma after every line)
  def challenges = ListMap[Int, Challenge](
    41 -> Challenge41,
    40 -> Challenge40,
    39 -> Challenge39,
    38 -> Challenge38,
    37 -> Challenge37,
    36 -> Challenge36,
    35 -> Challenge35,
    34 -> Challenge34,
    33 -> Challenge33,
    32 -> Challenge32,
    31 -> Challenge31,
    28 -> Challenge28,
//    28 -> Challenge28,
    26 -> Challenge26,
    25 -> Challenge25,
    24 -> Challenge24,
    23 -> Challenge23,
    22 -> Challenge22,
    21 -> Challenge21,
    20 -> Challenge20,
    19 -> Challenge19,
    18 -> Challenge18,
    17 -> Challenge17,
    16 -> Challenge16,
    15 -> Challenge15,
    14 -> Challenge14,
    13 -> Challenge13,
    12 -> Challenge12,
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
