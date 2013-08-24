package se.hardchee.S99

object Challenge41 extends Challenge {
  // Add solution here

  def go = {
    import arithmetic.S99Int._
    // Call internal methods with sample data here
    Some("\n" + List(
      printGoldbachList(9 to 20),
      printGoldbachListLimited(1 to 2000, 50)
    ).mkString("\n---\n") + "\n"
    )

  }
}
