package stackoverflow

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File
import StackOverflow._

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  private val lines   = sc.textFile("src/main/resources/stackoverflow/testdata.csv")
  private val raw     = rawPostings(lines).cache()

  // this test is very much tied to the content of testdata.csv.  If that
  // file changes, you likely have to change this test to match.  You probably
  // have to change some of the other tests as well.
  test("rawpostings functionality") {
    val data = raw.collect()

    //println(s"data = ")
    //data.foreach(println)

    assert(data.length === 10)
    assert(data(0) === Posting(1,27233496,None,None,6,Some("C#")))
    assert(data(4) === Posting(1,9419744,None,None,2,Some("Objective-C")))
    assert(data(9) === Posting(2,9005311,None,Some(9002525),0,None))
  }

  private val grouped = groupedPostings(raw).cache()

  test("groupedPostings functionality") {
    // In the test data there are 3 postings with "children".
    // We pull out the underlying data so we can confirm we got what we expected.
    // I use apply(0) instead of .collect()(0) because, to me, that "looks" strange.
    val group5484340 = grouped.filter(_._1 == 5484340).collect().apply(0)._2.toArray
    val group9002525 = grouped.filter(_._1 == 9002525).collect().apply(0)._2.toArray
    val group27233496 = grouped.filter(_._1 == 27233496).collect().apply(0)._2.toArray

    assert(group5484340.length === 1)
    val id54 = group5484340.map(_._2.id)
    assert(id54.contains(5494879), "child 5494879 not found")

    assert(group9002525.length === 3)
    val id90 = group9002525.map(_._2.id)
    assert(id90.contains(9003401), "child 9003401 not found")
    assert(id90.contains(9003942), "child 9003942 not found")
    assert(id90.contains(9005311), "child 9005311 not found")

    assert(group27233496.length === 1)
    val id27 = group27233496.map(_._2.id)
    assert(id27.contains(26875732), "child 26875732 not found")
  }

  private val scored  = scoredPostings(grouped).cache()

  test("scoredPostings functionality") {
    //println("scored = ")
    //scored.collect().foreach(println)

    val scored5484340 = scored.filter(_._1.id == 5484340).collect().apply(0)
    val scored9002525 = scored.filter(_._1.id == 9002525).collect().apply(0)
    val scored27233496 = scored.filter(_._1.id == 27233496).collect().apply(0)

    assert(scored5484340._2 === 1)  // from line 4 in testdata.csv
    assert(scored9002525._2 === 4)  // from line 8 in testdata.csv
    assert(scored27233496._2 === 3)  // from line 6 in testdata.csv
  }

  private val vectors = vectorPostings(scored).cache()

  test("vectorPostings functionality") {
    assert(vectors.count() === 3, "Incorrect number of vectors: " + vectors.count())
  }

  // You'd think I could have debug = true here.  But if I do, my
  // little amount of test data crashes the code that is attempting to
  // show the debug stuff.  I don't know why.  I also cannot use
  // sampleVectors(), so I pass a single point. I don't care, that was
  // Coursera code.  Moving on.
  private val means = kmeans(Array((0,0)), vectors, debug = false)

  // This one was too hard to figure out on my own.  So I cheated,
  // I'm using it as a regression test only.  I cheated by having the
  // code that Coursera accepted tell me the answer for the test
  // data, and then look for it here.  At least I can see if something
  // breaks.
  test("kmeans functionality") {
    //println("means = ")
    //means.foreach(println)
    assert(means(0)._1 === 216666)
    assert(means(0)._2 === 2)
  }

  private val results = clusterResults(means, vectors)

  test("clusterResults functionality") {
    // From test data, C# is the dominant language (look at the file).
    // 3 questions are C#, 1 is Objective-C, 1 C++.
    assert(results(0)._1 === "C#", "not expected dominant language")
    assert(results(0)._3 === 3, "number of questions is wrong")
  }

  test("median functionality") {
    // no numbers case.
    intercept[java.lang.IllegalArgumentException] {
      median(Seq())
    }

    // positive cases
    assert(median(Seq(14)) === 14, "median of 1 number must be that number")
    assert(median(Seq(12, 12)) === 12, "median of the same numbers is that number")
    assert(median(Seq(12, 16)) === 14, "median did not average the center points correctly")
    assert(median(Seq(-10, 3, 10)) === 3, "median didn't pull out the center number correctly")
    assert(median(Seq(1, 3, 5, 7)) === 4, "median didn't average the center points correctly")

    // negative cases
    assert(median(Seq(-4, 4, 3)) != 1, "median is calculating the average of the numbers, not the median (odd)")
    assert(median(Seq(-4, 4, 3, 1)) != 1, "median is calculating the average of the numbers, not the median (even)")
    assert(median(Seq(-4, 4, 3)) === 3, "median isn't sorting the input for odd sizes")
    assert(median(Seq(-4, 4, 2, -2)) === 0, "median isn't sorting the input for even sizes")
    assert(median(Seq(-4, 3, 4)) != -4, "median is off by 1 to the left for odd sizes")
    assert(median(Seq(-4, 3, 4)) != 4, "median is off by 1 to the right for odd sizes")
    assert(median(Seq(-4, -2, 0, 4)) != -3, "median is off by 1 to the left for even sizes")
    assert(median(Seq(-4, -2, 0, 4)) != 2, "median is off by 1 to the right for even sizes")
  }

}
