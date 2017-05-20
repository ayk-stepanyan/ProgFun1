package recfun

object Main {
  def main(args: Array[String]) {
    /*
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    def printBal(string: String, expectedRes: Boolean): Unit = {
      def result = if (expectedRes == balance(string.toList)) "Pass" else "Fail";
      println(string + ": " + result)
    }

    println("Parentheses Balancing")
    printBal("(if (zero? x) max (/ 1 x))                                      ", true)
    printBal("I told him (that it’s not (yet) done). (But he wasn’t listening)", true)
    printBal(":-)                                                             ", false)
    printBal("())(                                                            ", false)
    printBal("((())                                                           ", false)
    */

    println("Count Change")
    println(countChange(4, List(1,2)))
    println(countChange(300,List(5,10,20,50,100,200,500)))
    println(countChange(301,List(5,10,20,50,100,200,500)))
    println(countChange(300,List(500,5,50,100,20,200,10)))

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
    def balanceInternal(currentSum: Int, chars: List[Char]): Boolean = {
      if (currentSum < 0) false
      else if (chars.isEmpty) currentSum == 0
      else {
        val nextChar = chars.head
        if (nextChar == '(') balanceInternal(currentSum + 1, chars.tail)
        else if (nextChar == ')') balanceInternal(currentSum - 1, chars.tail)
        else balanceInternal(currentSum, chars.tail)
      }
    }

    balanceInternal(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def getCoinsLessThanMoney(money: Int, coins: List[Int]): List[Int] = {
      if (coins.isEmpty || coins.head <= money) coins
      else getCoinsLessThanMoney(money, coins.tail)
    }

    def countChangeIterationStep(money: Int, coins: List[Int]): Int = {
      val nextMoney = money - coins.head
      countChangeIteration(nextMoney, getCoinsLessThanMoney(nextMoney, coins)) + countChangeIteration(money, coins.tail)
    }

    def countChangeIteration(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (coins.isEmpty) 0
      else countChangeIterationStep(money, coins)
    }



    countChangeIteration(money, getCoinsLessThanMoney(money, coins.sortWith(_ > _)))
  }
}
