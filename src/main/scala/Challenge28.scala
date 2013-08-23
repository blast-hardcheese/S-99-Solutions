package se.hardchee.S99

object Challenge28 extends Challenge {
  def insert[T](shouldInsert: (List[T], List[T]) => Boolean, element: List[T], target: List[List[T]]): List[List[T]] = { target match {
      case x :: xs if(!shouldInsert(element, x)) => x :: insert(shouldInsert, element, xs)
      case x :: xs => element :: x :: xs
      case Nil => element :: Nil
    }
  }

  // Adding partitionInsert to explore a more concise way of writing insert, without matching
  def partitionInsert[T](elem: List[T], target: List[List[T]]): List[List[T]] = {
    val (l, r) = target.partition({ _.length < elem.length })
    l ::: elem :: r
  }

  def lsort[T](input: List[List[T]]): List[List[T]] = {
    def shouldInsert(elem: List[T], nextInList: List[T]): Boolean = nextInList.length >= elem.length 
    //input.foldLeft[List[List[T]]](List())({ case (last, next) => partitionInsert(next, last) })
    input.foldLeft[List[List[T]]](List())({ case (last, next) => insert(shouldInsert, next, last) })
  }

  def lsortFreq[T](input: List[List[T]]): List[List[T]] = {
    val priority = input.foldLeft[Map[Int, Int]](Map())({ (last, next) =>
      last.updated( next.length, last.getOrElse(next.length, 0) + 1)
    })

    input.foldLeft[List[List[T]]](List())({ (last, next) =>
      insert({ (elem: List[T], nextInList: List[T]) => priority(elem.length) < priority(nextInList.length) }, next, last)
    })
  }

  def go = {
    val before = new java.util.Date()
    val r = Some("\n" + List(
      lsort(    List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))),
      lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    ).mkString("\n") + "\n")
    val after = new java.util.Date()

    val milis = after.getTime() - before.getTime()
    println(milis)
    r
  }
}
