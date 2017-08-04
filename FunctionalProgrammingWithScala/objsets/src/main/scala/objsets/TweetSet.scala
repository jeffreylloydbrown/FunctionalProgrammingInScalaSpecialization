package objsets

import TweetReader._

/**
  * A class to represent tweets.
  */
class Tweet (val user: String, val text: String, val retweets: Int)
  {
    override
    def toString: String =
      {
        //"User: " + user + "\n" +
        "User: " + user + "; " +
          "Text: " + text + " [" + retweets + "]"
      }
  }

/**
  * This represents a set of objects of type `Tweet` in the form of a binary search
  * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
  * invariant which always holds: for every branch `b`, all elements in the left
  * subtree are smaller than the tweet at `b`. The elements in the right subtree are
  * larger.
  *
  * Note that the above structure requires us to be able to compare two tweets (we
  * need to be able to say which of two tweets is larger, or if they are equal). In
  * this implementation, the equality / order of tweets is based on the tweet's text
  * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
  * text from different users.
  *
  *
  * The advantage of representing sets as binary search trees is that the elements
  * of the set can be found quickly. If you want to learn more you can take a look
  * at the Wikipedia page [1], but this is not necessary in order to solve this
  * assignment.
  *
  * [1] http://en.wikipedia.org/wiki/Binary_search_tree
  */
abstract
class TweetSet
  {
    /**
      * Anything that subclasses TweetSet will not be empty, except for the Empty
      * class of course.  For example, a NonEmpty is *never* empty; it might have
      * left or right members that are Empty, but the object itself is *not* empty.
      * So provide the default for all to use, and Empty will override it.
      */
    def isEmpty: Boolean = false

    /**
      * This method takes a predicate and returns a subset of all the elements
      * in the original set for which the predicate is true.
      *
      * Question: Can we implement this method here, or should it remain abstract
      * and be implemented in the subclasses?
      *
      * Answer: Yes, because all filter() is doing is initializing an accumulator.
      * But that is likely not the answer the professor will say, because the
      * auxiliary function must be specific to each subclass.  Professor will say "no"
      * for this reason.
      */
    def filter (p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

    /**
      * This is a helper method for `filter` that propagates the accumulated tweets.
      * filterAcc() must be implemented by each subclass, because it is where
      * recursion happens when we're really dealing with a NonEmpty object.
      * In C++ I'd make this a protected method so nothing but our subclasses
      * can use it.  A 'protected' keyword exists, but my attempts to use it
      * failed:  I don't know enough about Scala yet.
      */
    def filterAcc (p: Tweet => Boolean, acc: TweetSet): TweetSet

    /**
      * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
      *
      * Question: Should we implement this method here, or should it remain abstract
      * and be implemented in the subclasses?
      *
      * Answer:  Yes, because all we're doing here is, again, initializing an
      * accumulator.  Note we use 'that' as the initialization, which cuts out a
      * lot of recursion.
      */
    def union (that: TweetSet): TweetSet = unionAcc(that)

    /**
      * This is a helper method for `union`.  It handles the recursion for
      * classes that need to recurse.
      */
    def unionAcc (acc: TweetSet): TweetSet

    /**
      * Returns the tweet from this set which has the greatest retweet count.
      *
      * Calling `mostRetweeted` on an empty set should throw an exception of
      * type `java.util.NoSuchElementException`.
      *
      * Question: Should we implement this method here, or should it remain abstract
      * and be implemented in the subclasses?
      *
      * Answer:  Since it is just returning the head of the descending tweet list,
      * and that doesn't depend on type, implement it here and have Empty override
      * it.
      */
    def mostRetweeted = descendingByRetweet().head

    /**
      * descendingByRetweet wants to put the least retweeted item last.  Recursion
      * builds lists backwards, so we need a method for finding the leastRetweeted
      * tweet.  descendingByRetweet can then use that to build the list in
      * descending order, without having to reverse it.
      */
    def leastRetweeted: Tweet

    // Unfortunately I cannot put this just in NonEmpty.  It isn't possible to
    // create a TweetSet initialization that can resolve for Empty.findLeastRetweeted
    // (even though I don't actually call it, the compiler doesn't know that),
    // and using NonEmpty for the initilization means left.findLeastRetweeted won't
    // resolve.  I tried left.leastRetweeted, which compiles but pukes at runtime
    // when the code attempts to use an Empty for that parameter.
    def findLeastRetweeted (tweet: Tweet): Tweet

    /**
      * Returns a list containing all tweets of this set, sorted by retweet count
      * in descending order. In other words, the head of the resulting list should
      * have the highest retweet count.
      *
      * Hint: the method `remove` on TweetSet will be very useful.
      * Question: Should we implement this method here, or should it remain abstract
      * and be implemented in the subclasses?
      */
    def descendingByRetweet(): TweetList =
      {
        def dbrAux (ts: TweetSet, tl: TweetList): TweetList =
          {
            if (ts.isEmpty) tl
            else
              {
                val least = ts.leastRetweeted
                dbrAux(ts.remove(least), new Cons(least, tl))
              }
          }

        dbrAux(this, Nil)
      }

    /**
      * The following methods are already implemented
      */

    /**
      * Returns a new `TweetSet` which contains all elements of this set, and the
      * the new element `tweet` in case it does not already exist in this set.
      *
      * If `this.contains(tweet)`, the current set is returned.
      */
    def incl (tweet: Tweet): TweetSet

    /**
      * Returns a new `TweetSet` which excludes `tweet`.
      */
    def remove (tweet: Tweet): TweetSet

    /**
      * Tests if `tweet` exists in this `TweetSet`.
      */
    def contains (tweet: Tweet): Boolean

    /**
      * This method takes a function and applies it to every element in the set.
      */
    def foreach (f: Tweet => Unit): Unit

    def printme (lbl: String) =
      {
        println(lbl + ": {")
        foreach(println)
        println("}")
      }
  }

class Empty extends TweetSet
  {
    override def isEmpty: Boolean = true

    /**
      * This is an Empty set.  So the filter expression doesn't matter
      * we just return the accumulator to the caller.
      */
    def filterAcc (p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

    /**
      * Union on an Empty set is simply the other set.
      */
    def unionAcc (acc: TweetSet): TweetSet = acc

    /**
      * An Empty TweetSet doesn't have a mostRetweeted count, and our spec
      * says to throw NoSuchElementException.
      */
    override def mostRetweeted: Tweet =
      {
        throw new java.util.NoSuchElementException("Empty.mostRetweeted isn't allowed")
      }

    /**
      * An Empty TweetSet doesn't have a leastRetweeted count, so puke if called.
      */
    def leastRetweeted: Tweet =
      {
        throw new java.util.NoSuchElementException("Empty.mostRetweeted isn't allowed")
      }

    /**
    * This method makes no sense for an Empty object, so puke if called.
    */
    def findLeastRetweeted (tweet: Tweet): Tweet =
      {
        throw new java.util.NoSuchElementException("Empty.findMostRetweeted isn't allowed")
      }

    /**
      * The following methods are already implemented
      */

    def contains (tweet: Tweet): Boolean = false

    def incl (tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

    def remove (tweet: Tweet): TweetSet = this

    def foreach (f: Tweet => Unit): Unit = ()
  }

// There's a key point to remember for this assignment, one I originally forgot.
// When the code is in any method of NonEmpty, you are *guaranteed* that left and
// right have a value.  The value might be an object of class Empty, but it is still
// a value.  That means you can safely and always call left.isEmpty or right.isEmpty
// to stop recursion.  The code below uses this to avoid having Empty have methods
// that make no sense for an Empty object.

class NonEmpty (elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet
  {
    /**
      * filterAcc() applies the predicate to the current element to decide if elem
      * should get added to the accumulator.  Then it calls itself first on the left
      * side of the tree, then on the right side.  (I used to have a separate method
      * that applied the predicate, but since it is a single, simple if statement I
      * threw the method out and just put the if statement here.)
      */
    def filterAcc (p: Tweet => Boolean, acc: TweetSet): TweetSet =
      {
        right.filterAcc(p, left.filterAcc(p, if (p(elem)) acc.incl(elem) else acc))
      }

    /**
      * Take the current element and add it to the accumulator.  Then call the
      * left and right side of the trees.
      */
    def unionAcc (acc: TweetSet): TweetSet =
      {
        right.unionAcc(left.unionAcc(acc.incl(elem)))
      }

    /**
      * leastRetweeted for a NonEmpty can start with the current elem instead
      * of a made-up tweet.
      */
    def leastRetweeted: Tweet = findLeastRetweeted(elem)

    /**
      * findLeastRetweeted() handles the recursion.  It receives a candidate
      * tweet.  If the current element has fewer retweets, it replaces the candidate
      * otherwise the rest of the calls use the passed candidate.
      */
    def findLeastRetweeted(tweet: Tweet): Tweet =
      {
        val candidate1 = if (elem.retweets < tweet.retweets) elem else tweet
        val candidate2 = if (left.isEmpty) candidate1 else left.findLeastRetweeted(candidate1)
        if (right.isEmpty) candidate2 else right.findLeastRetweeted(candidate2)
      }

    /**
      * The following methods are already implemented
      */

    def contains (x: Tweet): Boolean =
      {
        if (x.text < elem.text) left.contains(x)
        else if (elem.text < x.text) right.contains(x)
        else true
      }

    def incl (x: Tweet): TweetSet =
      {
        if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
        else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
        else this
      }

    def remove (tw: Tweet): TweetSet =
      {
        if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
        else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
        else left.union(right)
      }

    def foreach (f: Tweet => Unit): Unit =
      {
        f(elem)
        left.foreach(f)
        right.foreach(f)
      }
  }

trait TweetList
  {
    def head: Tweet

    def tail: TweetList

    def isEmpty: Boolean

    def foreach (f: Tweet => Unit): Unit =
      {
        if (!isEmpty)
          {
            f(head)
            tail.foreach(f)
          }
      }

    def printme (lbl: String) =
      {
        def printme_aux (tl: TweetList): Unit =
          {
            if (!tl.isEmpty)
              {
                println(tl.head)
                printme_aux(tl.tail)
              }
          }

        println(lbl + ": {")
        printme_aux(this)
        println("}")
      }
  }

object Nil extends TweetList
  {
    def head = throw new java.util.NoSuchElementException("head of EmptyList")

    def tail = throw new java.util.NoSuchElementException("tail of EmptyList")

    def isEmpty = true
  }

class Cons (val head: Tweet, val tail: TweetList) extends TweetList
  {
    def isEmpty = false
  }


object GoogleVsApple
  {
    def filterByKeyword (keywords: List[String], tweetData: TweetSet): TweetSet =
    {
      def fbkAux (keywords: List[String], acc: TweetSet): TweetSet =
      {
        if (keywords.isEmpty) acc
        else fbkAux(keywords.tail, acc.union(tweetData.filter(tw => tw.text.contains(keywords.head))))
      }
      fbkAux(keywords, new Empty)
    }

    val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
    val apple  = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

    lazy val googleTweets: TweetSet = filterByKeyword(google, TweetReader.allTweets)
    lazy val appleTweets : TweetSet = filterByKeyword(apple, TweetReader.allTweets)

    /**
      * A list of all tweets mentioning a keyword from either apple or google,
      * sorted by the number of retweets.
      */
    lazy
    val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet()
  }

object Main extends App
  {
    // Print the trending tweets

    GoogleVsApple.trending foreach println

  /*
  val set1 = new Empty
  val set2 = set1.incl(new Tweet("a", "a body", 20))
  val set3 = set2.incl(new Tweet("b", "b body", 20))
  val c = new Tweet("c", "c body", 7)
  val d = new Tweet("d", "d body", 9)
  val set4c = set3.incl(c)
  val set4d = set3.incl(d)
  val set5 = set4c.incl(d)

  set5.printme("set5")
  val tl: TweetList = set5.descendingByRetweet
  tl.printme("result from descendingByRetweet()")
  */
  }
