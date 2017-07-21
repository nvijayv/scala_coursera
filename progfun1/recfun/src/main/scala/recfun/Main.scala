package recfun

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
        if (r == 0 || r == 1) 1
        else if (c == 0) 1
        else if (c == r) 1
        else pascal(c-1, r-1) + pascal(c, r-1)
    }

    /**
      * Exercise 2
      */
    def balance(chars: List[Char]): Boolean = {
        def openClose(opens: Int, chars: List[Char]): Boolean = {
            if (chars.isEmpty) opens == 0
            else if (opens < 0) false
            else if (chars.head == '(')
                openClose(opens+1, chars.tail)
            else if (chars.head == ')')
                openClose(opens-1, chars.tail)
            else openClose(opens, chars.tail)
        }
        openClose(0, chars)
    }

    /**
      * Exercise 3
      */
    def countChange(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if (coins.isEmpty) 0
        else {
            (if (money >= coins.head) countChange(money - coins.head, coins) else 0) + countChange(money, coins.tail)
        }
    }
}
