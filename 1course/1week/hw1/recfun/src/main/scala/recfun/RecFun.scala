package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

    println(balance(List('(', ')', '(', ')')))

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if (c==0 || r==c) 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def loop(chars: List[Char], openCount: Int): Boolean = 
      if (chars.isEmpty) openCount == 0
      else
        val c = chars.head
        val newOpenCount = if (c == '(') openCount + 1
                           else if (c == ')') openCount - 1
                           else openCount
        openCount >= 0 && loop(chars.tail, newOpenCount)
    loop(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    if (money < 0 || coins.isEmpty) 0
    else if (money==0) 1
    else countChange(money-coins.head, coins) + countChange(money, coins.tail)
