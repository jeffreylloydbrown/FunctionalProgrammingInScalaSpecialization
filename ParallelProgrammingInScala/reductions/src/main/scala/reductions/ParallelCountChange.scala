package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   *
   *  @param money   The amount of currency we want to make change for.
   *  @param coins   A list of coin values available to make change with.  All values must be positive.
   *  @return        The number of ways of making change for `money` using the `coins` provided.  Unlike
   *                 the implementation for Functional Programming in Scala, we are to return 0 if
   *                 `money` < 0 or `coins` is empty.
   * @throws java.lang.IllegalArgumentException if any coin value is zero or negative.
   */
  @throws[java.lang.IllegalArgumentException]
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

    // Money cannot be negative, spec says to return zero instead of throwing an exception.
    if (money < 0) 0
    // Spec says to return 1 when money == 0, for which I disagree but hey I'm the student.
    else if (money == 0) 1
    // Spec says return 0 for an empty coins list.
    else if (coins.isEmpty) 0
    // Spec says nothing about invalid coin values, so doing what I originally did.
    else if (coins.exists(_ <= 0))
      throw new java.lang.IllegalArgumentException("a coin cannot be zero or negative")
    else {
      // We have money and some coin types, so validate the coins and hand off to worker.
      countChangeWorker(0, money, coins)
    }
  }


type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   *
   *  @param money      The amount of currency we want to make change for.
   *  @param coins      A list of coin values available to make change with.  All values must be positive.
   *  @param threshold  The Threshold function used to decide between parallel and sequential operation.
   *  @return           The number of ways of making change for `money` using the `coins` provided.  Unlike
   *                    the implementation for Functional Programming in Scala, we are to return 0 if
   *                    `money` < 0 or `coins` is empty.
   * @throws java.lang.IllegalArgumentException if any coin value is zero or negative.
   **/
  @throws[java.lang.IllegalArgumentException]
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (threshold(money, coins) || coins.isEmpty || money <= coins.length)  // stack overflows make me chop the space.
      countChange(money, coins)
    else
      {
        val (l, r) = parallel(parCountChange(money-coins.head, coins, threshold),
                              parCountChange(money, coins.tail, threshold))
        l + r
      }
  }

  /** Threshold heuristic based on the starting money.
    *
    * @param startingMoney  The starting amount of money used in the function we return (the threshold).
    * @return               A function that takes an amount and a list of coins, and returns true if
    *                       amount <= `startingMoney` * 0.67.
    **/
  def moneyThreshold(startingMoney: Int): Threshold =
    (money: Int, coins: List[Int]) =>
      money <= startingMoney * 2.0 / 3.0

  /** Threshold heuristic based on the total number of initial coins.
    *
    * @param totalCoins     The number of coins that represents the threshold.
    * @return               A function that takes an amount and a list of coins, and returns true if
    *                       coins.length <= `totalCoins` * 0.67.
    **/
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (money: Int, coins: List[Int]) =>
      coins.length <= totalCoins * 2.0 / 3.0

  /** Threshold heuristic based on the starting money and the initial list of coins.
    *
    * @param startingMoney  The starting amount of money used in the function we return (the threshold).
    * @param allCoins       The starting list of coins (not the length of the list).
    * @return               A function that takes an amount and a list of coins, and returns true if
    *                       money * coins.length <= `startingMoney` * `allCoins.length` * 0.5
    **/
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold =
    (money: Int, coins: List[Int]) =>
      money * coins.length <= startingMoney * allCoins.length / 2.0

}
