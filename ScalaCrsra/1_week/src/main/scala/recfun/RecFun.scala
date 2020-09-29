package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {

    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    //println("(ju)(a".toList.head)
    //println(balance("()()".toList))
    //println(countChange(4,List(1,2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c<1 || c==r  ) 1
    else {
      pascal(c-1,r-1) + pascal(c,r-1)
    }
  }



  /**
   * Exercise 2
   */
/* //Non Recursive
  def balance(chars: List[Char]): Boolean = {
    var c =0///counter to count parenthesis
    for (char <- chars) {
      if (char == '(') {
        c = c + 1
      }
      if (char == ')') {
        c = c - 1 // -1 for closing
      }
      if (c<0){
        return false
      }
    }
    //println(c)
    c == 0
  }
*/
  def balance(chars: List[Char]): Boolean = {

    def innerFn(chars: List[Char], cnt : Int) : Boolean = {
      if (chars.isEmpty && cnt==0) true
      else if (chars.head=='(') innerFn(chars.tail,cnt+1)
      else if (chars.head==')') {
        if (cnt >0) innerFn(chars.tail,cnt-1)
        else false
      }
      else innerFn(chars.tail,cnt)
    }
    innerFn(chars,0)

}

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money==0) 1
    else if (money < 0 || coins.isEmpty) 0
    else
      countChange(money-coins.head,coins)+ countChange(money,coins.tail)

  }
}
