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
    if (c > r)
      throw new IndexOutOfBoundsException("The column can't be greater than the row")

    // Basis:
    if (c == 0 || c == r) 1
    // Induction:
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def check(opened: Int, c: List[Char]): Boolean = {
      // Basis:
      if (c.isEmpty) opened == 0
      // Induction:
      else if (opened == 0 && c.head == ')') false
      else if (c.head == '(') check(opened + 1, c.tail)
      else if (c.head == ')') check(opened - 1, c.tail)
      else check(opened, c.tail)
    }

    check(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    // Basis:
    // If I have successfully divided up the money with the
    // given coins then I have found a successful combination
    // or "way" and I can signal this combo as a valid one to 
    // count.
    if (money == 0) 1
    // Basis:
    // If I run out of coins then this means that either I 
    // was unable to divide up the money with the available
    // coins so the combination of coins was inadequate so
    // don't count this as a valid way (returning zero mean
    // it will have no effect on the sum of valid combos). Or
    // if during the application of the denomination is breaking
    // down the currency brings the amount negative then it
    // is an invalid way of breaking the money apart.
    else if (coins.isEmpty || money < 0) 0
    // Induction:
    else
      // Apply the current coin in dividing up the money. This
      // will give us the remaining money to divide amongst 
      // our available denominations in the next recursive call. 
      // This recursive call essentially says "figure out all
      // the valid ways/combos with the current coin and the rest
      // of them and add them with all the valid combos with 
      // the other coins without the current coin." This should
      // cover all valid combos.
      countChange(money - coins.head, coins) +
        countChange(money, coins.tail)
  }
}
