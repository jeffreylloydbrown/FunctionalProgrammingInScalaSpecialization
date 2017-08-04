package stackoverflow

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import annotation.tailrec
import scala.reflect.ClassTag

/** A raw stackoverflow posting, either a question or an
  * answer
  */
case class Posting(postingType: Int,
                   id: Int,
                   acceptedAnswer: Option[Int],
                   parentId: Option[Int],
                   score: Int,
                   tags: Option[String]) extends Serializable {

  // The course homework assignment declares that Questions
  // have a value of 1, and Answers a value of 2.  Making
  // them constants here so any other code can use them.
  final val QUESTION: Int = 1
  final val ANSWER: Int = 2

  // Asking if a Posting is a question or an answer will be
  // a common thing.  Personally I'd have preferred they
  // use different case classes, to separate the subtypes.
  def isQuestion : Boolean = ( postingType == QUESTION )
  def isAnswer : Boolean = ( postingType == ANSWER )
}


/** The main class */
object StackOverflow extends StackOverflow {

  @transient lazy val conf: SparkConf =
    new SparkConf().setMaster("local[3]")
                   .setAppName("StackOverflow homework")
  @transient lazy val sc: SparkContext =
    new SparkContext(conf)
  // controls log4j, other choices:  "OFF", "DEBUG",
  // "INFO", "WARN", "ALL"
  sc.setLogLevel("ERROR")

  /** Main function */
  def main(args: Array[String]): Unit = {

    val lines   = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw     = rawPostings(lines)
    val grouped = groupedPostings(raw)
    val scored  = scoredPostings(grouped)
    val vectors = vectorPostings(scored)
    // assert(vectors.count() == 2121822,
    // "Incorrect number of vectors: " + vectors.count())

    // Since vectors is referenced multiple times, cache
    // it.  Had to move the call into vectorPostings(),
    // because main IS NOT CALLED by the grader.  Got this
    // tip from the discussion forum.
    //vectors.cache()

    val means   = kmeans(sampleVectors(vectors), vectors,
      debug = true)
    val results = clusterResults(means, vectors)
    printResults(results)
  }
}


/** The parsing and kmeans methods */
class StackOverflow extends Serializable {

  /** Languages */
  val langs =
    List(
      "JavaScript", "Java", "PHP", "Python", "C#", "C++",
      "Ruby", "CSS", "Objective-C", "Perl", "Scala",
      "Haskell", "MATLAB", "Clojure", "Groovy")

  /** K-means parameter: How "far apart" languages should
    * be for the kmeans algorithm?
    */
  def langSpread = 50000
  assert(langSpread > 0, "If langSpread is zero we can't recover the language from the input data!")

  /** K-means parameter: Number of clusters */
  def kmeansKernels = 45

  /** K-means parameter: Convergence criteria */
  def kmeansEta: Double = 20.0D

  /** K-means parameter: Maximum iterations */
  def kmeansMaxIterations = 120


  //
  //
  // Parsing utilities:
  //
  //

  /** Load postings from the given file */
  def rawPostings(lines: RDD[String]): RDD[Posting] =
    lines.map(line => {
      val arr = line.split(",")
      Posting(postingType =    arr(0).toInt,
              id =             arr(1).toInt,
              acceptedAnswer = if (arr(2) == "") None
                               else Some(arr(2).toInt),
              parentId =       if (arr(3) == "") None
                               else Some(arr(3).toInt),
              score =          arr(4).toInt,
              tags =           if (arr.length >= 6)
                                 Some(arr(5).intern())
                               else None)
    })


  /** Group the questions and answers together.
    *
    * @param postings   The RDD built from `rawPostings()`
    *                   or similar.
    * @return           An RDD that maps the Posting ID to
    *                   the Question and all its answers.
    */
  def groupedPostings(postings: RDD[Posting]): RDD[(Int, Iterable[(Posting, Posting)])] = {
    // First separate the questions and answers into 2
    // RDDs.  We need something to use as a key.  Every
    // answer has a question, indicated by its `parentId`.
    // Every question has an `id`, although some questions
    // might have no answers.  Create RDDs that use these
    // values, so we can then match them up.  I thought
    // answers would be a little tricky because the
    // `parentId` is an Option, meaning we'd have to
    // filter answers with no `parentId` away.  But
    // either there are no answers without questions,
    // or the map call ignores ones that are None.
    val questions = postings
      .filter(_.isQuestion)
      .map( posting => (posting.id, posting) )
    val answers = postings
      .filter(_.isAnswer)
      .map( posting => (posting.parentId.get, posting) )

    // Since some questions might not have answers, we
    // should use an outer join that includes every
    // question.  However, our return type is not
    // Iterable[(Posting, Option(Posting)] so I'm going
    // to do just a regular join.  Once joined, then we
    // can use `groupByKey()` to make the appropriate
    // iterable value.
    questions.join(answers).groupByKey()
  }


  /** Compute the maximum score for each posting */
  def scoredPostings(grouped: RDD[(Int, Iterable[(Posting, Posting)])]): RDD[(Posting, Int)] = {

    // We're given this function.  But it would be easier
    // if it took `Iterable[Posting]` as the argument, we
    // could just run a map over it and call max on the
    // collection of scores.  In fact I'm not sure why the
    // provider didn't just say `as.map(_.score).max`
    // instead of writing a loop.
    def answerHighScore(as: Array[Posting]): Int = {
      var highScore = 0
          var i = 0
          while (i < as.length) {
            val score = as(i).score
                if (score > highScore)
                  highScore = score
                  i += 1
          }
      highScore
    }

    // Every posting is held within the Iterable.  So the
    // first thing to do is pop the Iterable out, then
    // remove the various layers.  `flatMap` can do both.
    // We can then group by each posting, then map
    // `answerHighScore` over each of the answers---they
    // are in the values, so hello `mapValues`.
    grouped
      .flatMap(_._2)
      .groupByKey()
      .mapValues(v => answerHighScore(v.toArray))
  }


  /** Compute the vectors for the kmeans */
  def vectorPostings(scored: RDD[(Posting, Int)]): RDD[(Int, Int)] = {
    /** Return optional index of first language that
      * occurs in `tags`.
      */
    def firstLangInTag(tag: Option[String],
                       ls: List[String]): Option[Int] = {
      if (tag.isEmpty) None
      else if (ls.isEmpty) None
      else if (tag.get == ls.head) Some(0) // index: 0
      else {
        val tmp = firstLangInTag(tag, ls.tail)
        tmp match {
          case None => None
          case Some(i) => Some(i + 1) // index i in ls.tail => index i+1
        }
      }
    }

    // Tags are optional, but the map call seems to
    // handle avoiding the Nones for me.  I thought I
    // would have to filter for being defined, but that
    // proved unnecessary.  Map over what remains,
    // generating the pair of the spread language value
    // and the associated score.
    scored
      .map( p => (firstLangInTag(p._1.tags, langs).get *
                  langSpread, p._2)
          )
      .cache()
  }


  /** Sample the vectors */
  def sampleVectors(vectors: RDD[(Int, Int)]): Array[(Int, Int)] = {

    assert(kmeansKernels % langs.length == 0,
      "kmeansKernels should be a multiple of the number of languages studied.")
    val perLang = kmeansKernels / langs.length

    // http://en.wikipedia.org/wiki/Reservoir_sampling
    def reservoirSampling(lang: Int, iter: Iterator[Int], size: Int): Array[Int] = {
      val res = new Array[Int](size)
      val rnd = new util.Random(lang)

      for (i <- 0 until size) {
        assert(iter.hasNext,
          s"iterator must have at least $size elements")
        res(i) = iter.next
      }

      var i = size.toLong
      while (iter.hasNext) {
        val elt = iter.next
        val j = math.abs(rnd.nextLong) % i
        if (j < size)
          res(j.toInt) = elt
        i += 1
      }

      res
    }

    val res =
      if (langSpread < 500)
        // sample the space regardless of the language
        vectors.takeSample(false, kmeansKernels, 42)
      else
        // sample the space uniformly from each language
        // partition
        vectors
          .groupByKey
          .flatMap({
            case (lang, vectors) =>
              reservoirSampling(lang, vectors.toIterator,
                perLang).map((lang, _))
                   })
          .collect()

    assert(res.length == kmeansKernels, res.length)
    res
  }


  //
  //
  //  Kmeans method:
  //
  //

  /** The K-Means algorithm for cluster detection identifies
    * `k` clusters around which `n` data points cluster.
    * Given a set of data `vectors`, the algorithm is:
    *
    * 1. Pick `k` points called `means`.  This is called
    *    initialization.
    * 2. Associate each value in `points` with the mean
    *    that is closest to it.  We obtain `k` clusters of
    *    points, and we refer to this process as classifying
    *    the points.
    * 3. Update each mean to have the average value of the
    *    corresponding cluster.
    * 4. If the `k` means have changed significantly (that
    *    is, if the change is larger than some threshold
    *    `eta`), go back to step 2.  If they did not, we
    *    say that the algorithm converged.
    * 5. The `k` means represent different clusters---every
    *    point is in the cluster corresponding to the
    *    closest mean.
    *
    * Making great use of this algorithm requires picking
    * the initial `means` "smartly" and selecting an
    * appropriate value of `eta`.  Both "smartly" and
    * "appropriate" are vague on purpose....
    *
    * @param means    The starting collection of means.
    * @param vectors  The collection of `(lang, score)`
    *                 pairs.
    * @param iter     Call counter to see if `kmeans()`
    *                 isn't converging.  Will make up to
    *                 `kmeansMaxIterations` attempts.
    *                 On initial call, leave parameter out.
    * @param debug    `true` to print info on each
    *                 iteration, defaults to `false`.
    * @return         Final collection of means that
    *                 `vectors` cluster around.
    */
  @tailrec final def kmeans(means: Array[(Int, Int)],
                            vectors: RDD[(Int, Int)],
                            iter: Int = 1,
                            debug: Boolean = false): Array[(Int, Int)] = {
    val newMeans = means.clone()

    val means_collection = vectors                // RDD[(lang, score)] ->
      // Step 2: classify
      .map { p => ( findClosest(p, means), p ) }  // RDD[(index in means, (lang, score))] ->
      .groupByKey()                               // RDD[(index in means, Iterable[(lang, score)])] ->
      // Step 3: compute the updated mean
      .mapValues(averageVectors)                  // RDD[(index in means, (avg. lang, avg. score)]
      .collect()

    // Since distributed data can be returned in any
    // order, we have to transfer the results into the
    // correct locations in the original array.
    // I wondered about using .sortBy(_._1), and I think
    // it's possible for means_collection to not be the
    // same size as newMeans, so a straight replacement
    // isn't safe.  I can always try it after everything
    // is working.  If a foreach loop remains here, it
    // didn't work.
    means_collection.foreach {
      case (idx, (lang, score)) => newMeans(idx) = (lang, score)
    }

    // Step 4:  If we have not converged or made too many
    // attempts, go back to Step 2.
    val distance = euclideanDistance(means, newMeans)

    if (debug) {
      println(s"""Iteration: $iter
                 |  * current distance: $distance
                 |  * desired distance: $kmeansEta
                 |  * means:""".stripMargin)
      for (idx <- 0 until kmeansKernels)
      println(
        f"   ${means(idx).toString}%20s ==> " +
        f"${newMeans(idx).toString}%20s  " +
        f"  distance: ${euclideanDistance(means(idx), newMeans(idx))}%8.0f")
    }

    if (converged(distance))
      newMeans  // Step 5:  return the collection of means
    else if (iter < kmeansMaxIterations)
      // back to Step 2
      kmeans(newMeans, vectors, iter + 1, debug)
    else {
      println("Reached max iterations!")
      newMeans
    }
  }


  //
  //
  //  Kmeans utilities:
  //
  //

  /** Decide whether the kmeans clustering converged */
  def converged(distance: Double) =
    distance < kmeansEta


  /** Return the euclidean distance between two points.
    *
    * @param v1   A `(lang, score)` pair.
    * @param v2   Another `(lang, score)` pair based
    *             on the same coordinate system.
    * @return     The sum of the squares of the `lang`
    *             (the x axis) distance and the `score`
    *             (the y axis) distance.
    */
  def euclideanDistance(v1: (Int, Int),
                        v2: (Int, Int)): Double = {
    val part1 = (v1._1 - v2._1).toDouble * (v1._1 - v2._1)
    val part2 = (v1._2 - v2._2).toDouble * (v1._2 - v2._2)
    part1 + part2
  }

  /** Return the euclidean distance between two collections
    * of points.  `a1` and `a2` must be the same size.
    *
    * @param a1     An array of `(lang, score)` pairs
    * @param a2     Another array of `(lang, score)` pairs
    *               based on the same coordinate system.
    * @return       The sum of the distances between each
    *               pair-wise point in `a1` and `a2`.
    */
  def euclideanDistance(a1: Array[(Int, Int)],
                        a2: Array[(Int, Int)]): Double = {
    assert(a1.length == a2.length)
    var sum = 0d
    var idx = 0
    while(idx < a1.length) {
      sum += euclideanDistance(a1(idx), a2(idx))
      idx += 1
    }
    sum
  }

  /** Return the index of the closest `mean` to a
    * particular `(lang, score)` vector.
    *
    * @param p        The vector to examine.
    * @param centers  The collection of current means
    *                 to search.
    * @return         The index into `centers`
    *                 (the collection of means) of
    *                 the mean closest to `p`.
    */
  def findClosest(p: (Int, Int),
                  centers: Array[(Int, Int)]): Int = {
    var bestIndex = 0
    var closest = Double.PositiveInfinity
    for (i <- 0 until centers.length) {
      val tempDist = euclideanDistance(p, centers(i))
      if (tempDist < closest) {
        closest = tempDist
        bestIndex = i
      }
    }
    bestIndex
  }


  /** Average the vectors
    *
    * @param ps   Collection of (lang, score) pairs
    * @return     A single pair, the average lang value and
    *             the average score value, as
    *             (avg. lang, avg. score).
    */
  def averageVectors(ps: Iterable[(Int, Int)]): (Int, Int) =
  {
    val iter = ps.iterator
    var count = 0
    var comp1: Long = 0
    var comp2: Long = 0
    while (iter.hasNext) {
      val item = iter.next
      comp1 += item._1
      comp2 += item._2
      count += 1
    }
    ((comp1 / count).toInt, (comp2 / count).toInt)
  }




  //
  //
  //  Displaying results:
  //
  //

  /** Summarize the results for each cluster.  For each
    * cluster, compute:
    *
    * - the dominant language in the cluster
    * - the percent of answers that belong to the
    *   dominant language
    * - the size of the cluster (the number of questions
    *   it contains)
    * - the median (not the mean/average!) of the highest
    *   answer scores
    *
    * @param means    The final `means`, typically from
    *                 calling `kmeans`.
    * @param vectors  The original `(lang, score)` pairs.
    *                 Here, `lang` will be spread so we
    *                 must recover the original language
    *                 code.
    * @return         A collection of tuples providing
    *                 data on each cluster of vectors.
    */
  def clusterResults(means: Array[(Int, Int)],
                     vectors: RDD[(Int, Int)]): Array[(String, Double, Int, Int)] = {

    // Start by reclassifying the original vectors,
    // but this time we use the really good guesses
    // we previously computed.  Remember that in
    // `closestGrouped`, each `lang` is actually
    // the spread lang value.
    val closest = vectors                       // RDD[(lang, score)] ->
      .map(p => (findClosest(p, means), p))     // RDD[(index into means, (lang, score)] ->
    val closestGrouped = closest.groupByKey()   // RDD[(index into means, Iterable[(lang, score)])] ->

    // Now summarize each cluster.  We could have
    // jettisoned the index above, after the groupByKey()
    // as we no longer need it.  However, we can save
    // an operation by using mapValues() instead of map().
    val summary = closestGrouped.mapValues { vs =>

      // `vs` is an Iterable[(spread lang, score)].

      // The most common language in the cluster
      // will have the most occurrences of the
      // spread lang value.  We can group by
      // the spread lang value, then find the
      // maximum # of occurrences (which is the
      // size of each grouped _2).
      val ( mostCommonSpreadLang, mclOccurrences ) =
        vs.groupBy(_._1)
          .mapValues(_.size)
          .maxBy(_._2)

      // Most common language in the cluster
      val langLabel: String   = langs(mostCommonSpreadLang / langSpread)

      // Cluster size is simply the size of vs.
      val clusterSize: Int    = vs.size

      // Most common language percent is mclOccurrences /
      // occurrences * 100.0D.  The total occurrences
      // is just the size of the cluster.
      val langPercent: Double =
        100.0d * mclOccurrences / clusterSize.toDouble

      // To get the median score, we have to extract
      // all the scores in order to call a calculation
      // procedure.
      val medianScore: Int    =
        median(vs.map(_._2).toSeq)

      (langLabel, langPercent, clusterSize, medianScore)
    }                                           // RDD[(index into means, (label, %, size, median))]

    // Pull the median collection back from Spark,
    // throw away the index, and sort by the median score.
    summary.collect().map(_._2).sortBy(_._4)
  }

  /** Compute the median value of a collection of integers.
    *
    * To do so, first sort the collection in ascending order.
    * The location of the median depends on if there are an
    * odd number or even number of items in the collection.
    *
    * If odd, the median value is at size/2.  Thanks to
    * integer arithmetic, it will automatically round down
    * For instance, the index of the median in a 3-element
    * collection is 3/2 = 1 in integer arithmetic.  For a
    * 1-element, 1/2 = 0; for a 5-element collection, 5/2 = 2.
    *
    * If even, the median value is the average of the 2
    * values surrounding the middle point.  For a 2-element
    * collection, median = average(c(0), c(1)).  For a 4-
    * element collection, median = average(c(1), c(2)).
    * For an 8-element collection, median = average(c(3), c(4)).
    * In other words, average(c(mid-1), c(mid)), where
    * mid = size/2.
    *
    * @param c  the collection to compute the median for.
    * @return   The median value in the collection.
    * @throws java.lang.IllegalArgumentException if `c` is empty.
    */
  @throws[java.lang.IllegalArgumentException]
  def median(c: Seq[Int]): Int = {
    require(c.nonEmpty, "cannot compute a median of no numbers")
    if (c.size == 1) c(0)
    else {
      val sorted = c.sortWith(_ < _)
      val mid = sorted.length / 2

      if (sorted.size % 2 == 0) // even
        (sorted(mid - 1) + sorted(mid)) / 2
      else // odd
        sorted(mid)
    }
  }

  /** Takes a collection of tuples created by
    * `clusterResults()` and prints them to stdout.
    */
  def printResults(results: Array[(String, Double, Int, Int)]): Unit = {
    println("Resulting clusters:")
    println("  Score  Dominant language (%percent)  Questions")
    println("================================================")
    for ((lang, percent, size, score) <- results)
      println(f"${score}%7d  ${lang}%-17s (${percent}%-5.1f%%)      ${size}%7d")
  }
}
