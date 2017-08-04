package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelCountChange._

@RunWith(classOf[JUnitRunner])
class ParallelCountChangeSuite extends FunSuite {

  test("countChange should return 0 for money < 0") {
    def check(money: Int, coins: List[Int]) = 
      assert(countChange(money, coins) == 0,
        s"countChange($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("countChange should return 1 when money == 0") {
    def check(coins: List[Int]) = 
      assert(countChange(0, coins) == 1,
        s"countChange(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("countChange should return 0 for money > 0 and coins = List()") {
    def check(money: Int) = 
      assert(countChange(money, List()) == 0,
        s"countChange($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("negative coin value throws exception") {
    intercept[java.lang.IllegalArgumentException] {
      countChange(100, List(5,1,-5))
    }
  }

  test("zero coin value throws exception") {
    intercept[java.lang.IllegalArgumentException] {
      countChange(91, List(5,0))
    }
  }

  test("all coins are larger than money so no change") {
    assert(countChange(20, List(25,50,100)) === 0)
  }

  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange:  1 coin means 1 way iff it divides") {
    assert(countChange(4,List(2)) === 1)
  }

  test("countChange: 1 coin means 0 ways iff it doesn't divide") {
    assert(countChange(4,List(3)) === 0)
  }

  test("countChange should work when there is only one coin") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("countChange should work for multi-coins") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }

  test("totalCoinsThreshold == true when number of coins < 2/3 of initial number of coins") {
    // 3 coins is true, 4 is false for 5 starting coins.
    val thresh = 5
    assert(totalCoinsThreshold(thresh)(100, List()) === true)
    assert(totalCoinsThreshold(thresh)(100, List(5)) === true)
    assert(totalCoinsThreshold(thresh)(100, List(5,10)) === true)
    assert(totalCoinsThreshold(thresh)(100, List(5,10,25)) === true)
    assert(totalCoinsThreshold(thresh)(100, List(5,10,25,50)) === false)
    assert(totalCoinsThreshold(thresh)(100, List(5,10,25,50,100)) === false)
    assert(totalCoinsThreshold(thresh)(100, List(5,10,25,50,100,500)) === false)
  }

  test("totalCoinsThreshold == true when number of coins == 2/3 initial number of coins (checks for int arithmetic)") {
    // 4 coin is true, 5 is false for 6 starting coins.
    val thresh = 6
    assert(totalCoinsThreshold(thresh)(100, List()) === true)
    assert(totalCoinsThreshold(thresh)(100, List(5)) === true)
    assert(totalCoinsThreshold(thresh)(100, List(5,10)) === true)
    assert(totalCoinsThreshold(thresh)(100, List(5,10,25)) === true)
    assert(totalCoinsThreshold(thresh)(100, List(5,10,25,50)) === true)
    assert(totalCoinsThreshold(thresh)(100, List(5,10,25,50,100)) === false)
    assert(totalCoinsThreshold(thresh)(100, List(5,10,25,50,100,500)) === false)
    assert(totalCoinsThreshold(thresh)(100, List(5,10,25,50,100,500,1000)) === false)
  }

  test("moneyThreshold == true when money < 2/3 of starting money") {
    // Pick a threshold that isn't divisible by 3.
    val thresh = 100
    assert(moneyThreshold(thresh)(0, List(5,10,25)) === true)
    assert(moneyThreshold(thresh)(100, List(5,10,25)) === false)
    assert(moneyThreshold(thresh)(65, List(5,10,25)) === true)
    assert(moneyThreshold(thresh)(66, List(5)) === true)
    assert(moneyThreshold(thresh)(67, List(5)) === false)
    assert(moneyThreshold(thresh)(68, List(5)) === false)
  }

  test("moneyThreshold == true when money == 2/3 of starting money (checks for int arithmetic)") {
    // Pick a threshold that easily divides by 3.
    val thresh = 300
    assert(moneyThreshold(thresh)(0, List(5,10,25)) === true)
    assert(moneyThreshold(thresh)(300, List(5,10,25)) === false)
    assert(moneyThreshold(thresh)(199, List(5,10,25)) === true)
    assert(moneyThreshold(thresh)(200, List(5)) === true)
    assert(moneyThreshold(thresh)(201, List(5)) === false)
  }

  test("combinedThreshold == true when number of coins * money < 1/2 initial number of coins * starting money") {
    // Picking prime number thresholds on purpose to expose any integer math problems.
    // 11 * List(1,5,10).length / 2.0 = 16.5
    val starting_money = 11
    val starting_coins = List(1,5,10)
    assert(combinedThreshold(starting_money,starting_coins)(0, List(5,10,25)) === true)   // 0 -> true
    assert(combinedThreshold(starting_money,starting_coins)(8, List(5,10)) === true)      // 16 -> true
    assert(combinedThreshold(starting_money,starting_coins)(17, List(5)) === false)       // 17 -> false
    assert(combinedThreshold(starting_money,starting_coins)(17, List()) === true)         // 0 (no coins) -> true
  }

  test("combinedThreshold == true when number of coins * money == 1/2 initial number of coins * starting money") {
    // This time pick multiples of 2.
    // 12 * List(1,5,10,25).length / 2.0 = 24
    val starting_money = 12
    val starting_coins = List(1,5,10,25)
    assert(combinedThreshold(starting_money,starting_coins)(0, List(5,10,25)) === true)   // 0 -> true
    assert(combinedThreshold(starting_money,starting_coins)(23, List(5)) === true)        // 23 -> true
    assert(combinedThreshold(starting_money,starting_coins)(6, List(1,5,10,25)) === true) // 24 -> true
    assert(combinedThreshold(starting_money,starting_coins)(25, List(1)) === false)       // 25 -> false
    assert(combinedThreshold(starting_money,starting_coins)(25, List()) === true)         // 0 (no coins) -> true
  }

  test("parCountChange with combinedThreshold produces correct output with 1 coin and amount < coin value") {
    val starting_money = 10
    val starting_coins: List[Int] = List(25)
    assert(parCountChange(starting_money, starting_coins, combinedThreshold(starting_money, starting_coins)) === 0)
  }

  test("parCountChange with moneyThreshold produces correct output with 1 coin and amount < coin value") {
    val starting_money = 10
    val starting_coins: List[Int] = List(25)
    assert(parCountChange(starting_money, starting_coins, moneyThreshold(starting_money)) === 0)
  }

  test("parCountChange: sorted CHF") {
    val money = 300
    val coins = List(5,10,20,50,100,200,500)
    assert(parCountChange(money, coins, moneyThreshold(money)) === 1022)
    assert(parCountChange(money, coins, totalCoinsThreshold(coins.length)) === 1022)
    assert(parCountChange(money, coins, combinedThreshold(money, coins)) === 1022)
  }

  test("parCountChange: no pennies") {
    val money = 301
    val coins = List(5,10,20,50,100,200,500)
    assert(parCountChange(money, coins, moneyThreshold(money)) === 0)
    assert(parCountChange(money, coins, totalCoinsThreshold(coins.length)) === 0)
    assert(parCountChange(money, coins, combinedThreshold(money, coins)) === 0)
  }

  test("parCountChange: unsorted CHF") {
    val money = 300
    val coins = List(500,5,50,100,20,200,10)
    assert(parCountChange(money, coins, moneyThreshold(money)) === 1022)
    assert(parCountChange(money, coins, totalCoinsThreshold(coins.length)) === 1022)
    assert(parCountChange(money, coins, combinedThreshold(money, coins)) === 1022)
  }

  test("parCountChange with moneyThreshold produces correct output when there are 2 coins and amount == 1") {
    val starting_money = 1
    val nickel_dime: List[Int] = List(5,10)
    val penny_nickel: List[Int] = List(1,5)
    assert(parCountChange(starting_money, nickel_dime, moneyThreshold(starting_money)) === 0)    // needs a penny
    assert(parCountChange(starting_money, penny_nickel, moneyThreshold(starting_money)) === 1)   // has a penny
  }

  test("parCountChange with moneyThreshold should produce correct result when there is only 1 coin and money == coin value") {
    val starting_money = 5
    assert(parCountChange(starting_money, List(starting_money), moneyThreshold(starting_money)) === 1)
  }

  test("parCountChange with combinedThreshold should produce correct result when there is only 1 coin and money == coin value") {
    val starting_money = 5
    val starting_coins: List[Int] = List(5)
    assert(parCountChange(starting_money, starting_coins, combinedThreshold(starting_money, starting_coins)) === 1)
  }

  test("parCountChange with totalCoinsThreshold should produce correct output when there are 2 coins and the amount == 1") {
    val nickel_dime: List[Int] = List(5,10)
    val penny_nickel: List[Int] = List(1,5)
    assert(parCountChange(1, nickel_dime, totalCoinsThreshold(nickel_dime.length)) === 0)     // needs a penny
    assert(parCountChange(1, penny_nickel, totalCoinsThreshold(penny_nickel.length)) === 1)   // has a penny
  }

  /*
  test("6 parallels") {
    parCountChange(16, List(1), moneyThreshold(16))
  }
  */

}
