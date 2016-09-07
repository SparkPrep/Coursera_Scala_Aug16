package recfun

object Main {
  /**
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    **/


/**
  def main(args: Array[String]) {
     println(balance("(if (zero? x) max (/ 1 x))".toList))
     println(balance(":-)".toList))
     println(balance("())(".toList))
      }

  **/

  def main(args: Array[String]) {
    println(countChange(4, List(1,2)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r)1 else pascal(c,r-1)+ pascal(c-1,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def valid (expr: List[Char], count:Int): Int =
        if (expr.isEmpty)
          count

        else if (expr.head == '(')
          valid(expr.tail,count+1)

        else if (expr.head == ')' && count > 0 )
          valid(expr.tail,count -1)

        else
          valid(expr.tail,count)


      if (chars.count(_.equals('(')) == chars.count(_.equals(')'))) valid(chars,0) == 0
      else false
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def recurs(money: Int, coins: List[Int], combo: Int): Int =
        if(money < 0) combo
        else if(coins.isEmpty) {
          if(money == 0) combo + 1 else combo
        }
        else recurs(money, coins.tail, combo) + recurs(money-coins.head, coins, combo)
      recurs(money, coins, 0)
    }



  }
