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

    // Since both column and row must be zero or greater, the spec really should have used UInt instead of Int.
    // Oh well, test them both and throw an invalid argument exception if either is negative.  Also, column must
    // always be no larger than row, or you're outside the triangle.
    if (c < 0)      throw new java.lang.IllegalArgumentException("column must not be negative")
    else if (r < 0) throw new java.lang.IllegalArgumentException("row must not be negative")
    else if (c > r) throw new java.lang.IllegalArgumentException("column cannot be larger than row")

    // OK, parameters are valid.  The edges of the triangle are always 1.  Column 0 is
    // one of the edges.  The other edge is where row == column.  We don't have to check
    // for row == 0 or == 1 because when row == 0 column must also be zero, and when row
    // == 1 column can be at most 1.  So the additional cases really aren't necessary.
    if (c == 0 || c == r) {
      1
    }
    else {
      // The recursive case:  The value is the sum of the value above and the value above and left in
      // the triangle.
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
    * Exercise 2
    *
    * The main routine will have 1 purpose:  to call the worker routine while adding a way
    * to track levels.  Yes it can and should be an inner function.  But I find it harder
    * to read the comments, and separate the 2 routines.  Will change it if I get docked on the grade.
    */
  def balance(chars: List[Char]): Boolean = balanceWorker(0, chars)

  // balanceWorker is the guts of the balance check.  It recursively walks the characters looking
  // for parens.  Left parens increase the level, right parens decrease the level.  If the level
  // ever goes negative, we know we aren't balanced so answer "false".  If we get to an empty
  // list of characters and level is zero, we are balanced and return true; if level isn't zero
  // then we're unbalanced so return false.
  //
  // Note this handles the degenerate case of an empty string as well as no parens in the string.
  // Either way, the list will eventually be empty and level will be zero, so we'll get a true return.
  // Therefore the balance() method doesn't have to test for an empty list.
  def balanceWorker(level: Int, chars: => List[Char]): Boolean = {
    if (level < 0) false
    else if (chars.isEmpty) (level == 0)    // notice that is a Boolean expression!
    else
      balanceWorker(level + checkForParen(chars.head), chars.tail)
  }

  // Examine the passed character.  If it is a left paren, return +1.  If it is a right paren,
  // return -1.  For anything else, return 0.  Using this routine makes balanceWorker more
  // readable.
  def checkForParen(c: Char) = if (c == '(') +1 else if (c == ')') -1 else 0

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    // countChangeWorker() handles the recursion.  It doesn't do error checking because
    // countChange() handles all the validation before starting the main calculations.
    //
    // We have to perform a double recursion to solve this.  One recursion handles reducing
    // the money towards zero, attempting to find exact change.  If we hit zero, we've found
    // a valid way to make change and increment the count.  If money goes negative, the
    // way is not valid so return the same count.  If the coins list is empty, money wasn't
    // zero by the time we test that so the path isn't valid.
    //
    // The other recursion starts with the same amount of money, but removes a candidate
    // coin.  Together, both ways explore the entire solution space, where the money is
    // repeatedly pushed toward zero with all combinations of coins.
    //
    // There is a short-circuit that's possible, when there is 1 coin.  In this case,
    // you can only make change by repeatedly using the same type of coin.  Therefore,
    // if the money is evenly divisible by the coin, you can make change.  Otherwise you
    // can't.  Same rule:  making change increases the count, not making change doesn't
    // increase the count.  This short-circuit removes part of the search tree formed by
    // all the recursions, making the overall program faster.
    def countChangeWorker(count: Int, money: Int, coins: List[Int]): Int = {
      if (money == 0) count+1
      else if (money < 0) count
      else if (coins.isEmpty) count     // test after checking money so don't need "&& money != 0"
      else if (coins.tail.isEmpty) {
        // Only 1 coin, so only way to make change is if that coin divides
        // into the money with no remainder.
        if (coins.head > money) count   // the coin is too large to make change for the money
        else if ((money % coins.head) == 0) count+1
        else count
      }
      else {
        countChangeWorker(count, money-coins.head, coins) + countChangeWorker(count, money, coins.tail)
      }
    }

    // countChange() body:

    // Money cannot be negative, so test and complain.
    if (money < 0) throw new java.lang.IllegalArgumentException("money cannot be negative")
    // No money or no coins means no change possible.
    else if (money == 0 || coins.isEmpty) 0
    else {
      // We have money and some coin types, so validate the coins and hand off to worker.
      validateCoinList(coins)
      countChangeWorker(0, money, coins)
    }
  }

  // Instead of validating individual coins possibly thousands of times inside all the
  // recursive calls, do it 1 time per coin outside countChangeWorker().  That way
  // countChangeWorker() won't have to check stuff.  This routine throws an exception
  // if it finds a problem, so it always returns true (since I don't know how to do a
  // procedure yet.)
  def validateCoinList(coins: List[Int]): Boolean = {
    if (coins.isEmpty) true
    else if (coins.head <= 0) {
      throw new java.lang.IllegalArgumentException("a coin cannot be zero or negative")
    }
    else validateCoinList(coins.tail)
  }
}
