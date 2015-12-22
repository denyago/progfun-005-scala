package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(c: List[Char], bal: Int): Int = {
      if (c.isEmpty || bal < 0) return bal

      c.head match {
        case ')' => loop(c.tail, bal - 1)
        case '(' => loop(c.tail, bal + 1)
        case _   => loop(c.tail, bal)
      }
    }

    loop(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, m: Int): Int = {
      if (money < 0 || m < 0) return 0
      if (money == 0) return 1

      loop(money, m - 1) + loop(money - coins(m), m)
    }

    loop(money, coins.size - 1)
  }
}
