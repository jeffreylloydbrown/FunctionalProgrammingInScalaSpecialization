package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("set5 contains 'c body'") {
    new TestSets {
      assert(set5.contains(new Tweet("c", "c body", 7)))
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      /*
      println("set2=")
      set2.foreach(println)
      println()
      */

      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set5") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("Empty.mostRetweeted is an exception")
    {
      intercept[java.util.NoSuchElementException]
        {
          val e = new Empty
          e.mostRetweeted
        }
    }

  test("Empty.leastRetweeted is an exception")
  {
    intercept[java.util.NoSuchElementException]
      {
        val e = new Empty
        e.leastRetweeted
      }
  }

  test("Empty.findLeastRetweeted is an exception")
  {
    intercept[java.util.NoSuchElementException]
      {
        val e = new Empty
        e.findLeastRetweeted(new Tweet("","",-1))
      }
  }

  test("Empty.contains is false")
  {
    val e = new Empty
    assert(e.contains(new Tweet("","",-1)) === false)
  }

  test("Empty.remove returns an empty set")
  {
    val e = new Empty
    assert(e.remove(new Tweet("a", "a body", 20)).isEmpty === true)
  }

  test("most set2 retweets == 20")
  {
    new TestSets
      {
        assert(set2.mostRetweeted.retweets === 20)
      }
  }

  test("look for something other than 20")
  {
    new TestSets
      {
      val jb = set1.incl(new Tweet("jb", "j body", 16))
      val km = jb.incl(new Tweet("km", "big body", 14))
      assert(km.mostRetweeted.retweets === 16)
      }
  }

  test("set5 has 20 for most retweets")
    {
      new TestSets
        {
          set5.printme("set5")   // solely for coverage
          assert(set5.mostRetweeted.retweets === 20)
        }
    }

  test("set5 has 7 for fewest retweets")
  {
    new TestSets
      {
        assert(set5.leastRetweeted.retweets === 7)
      }
  }

  test("Nil.head is an exception")
  {
    intercept[java.util.NoSuchElementException]
      {
        Nil.head
      }
  }

  test("Nil.tail is an exception")
  {
    intercept[java.util.NoSuchElementException]
      {
        Nil.tail
      }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet()
      trends.printme("trends is set5 in descending order")
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
}
