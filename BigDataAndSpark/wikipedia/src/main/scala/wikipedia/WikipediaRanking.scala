package wikipedia

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._

import org.apache.spark.rdd.RDD

case class WikipediaArticle(title: String, text: String) {
  /**
    * @return Whether the text of this article mentions `lang` or not
    * @param lang Language to look for (e.g. "Scala").  It is case sensitive.
    */
  def mentionsLanguage(lang: String): Boolean = text.split(' ').contains(lang)

  /** Returns the list of languages that appear in a `WikipediaArticle`.  This is a helper function
    * for `makeIndex()` and `rankLangsReduceByKey()`
    *
    * @param langs    List of language names to search for.
    * @return         List of languages that are mentioned in `article`.
    */
  def mentionsLangs(langs: List[String]): List[String] =
    langs.filter( {lang: String => this.mentionsLanguage(lang)} )
}

object WikipediaRanking {

  val langs = List(
    "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
    "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

  /** For this assignment, we are to run Spark in "local" mode.  This means our full
    * Spark application will run on one node only, locally, on this machine along with
    * our application code.
    *
    * We have to first create a configuration, then use that configuration object to
    * initialize a SparkContext.  We will choose to try running on 3 cores: the [n]
    * after `local` indicates this.  Finally, our source RDD will be constructed from a
    * data file stored in our resource area.  We will cache that RDD so we don't have to
    * reload the data.
    */
  val conf: SparkConf = new SparkConf().setMaster("local[3]")
                                       .setAppName("Wikipedia homework")
  val sc: SparkContext = new SparkContext(conf)
  sc.setLogLevel("ERROR")     // controls log4j, other choices:  "OFF", "DEBUG", "INFO", "WARN", "ALL"

  // Hint: use a combination of `sc.textFile`, `WikipediaData.filePath` and `WikipediaData.parse`
  /** `sc.textfile()` reads a text file and returns `RDD[String]`.  `WikipediaData.parse()` turns a
   *  `String` into a `WikipediaArticle`, so if I map it over `RDD[String]`, I get the
   *  `RDD[WikipediaArticle]` I'm looking for.  Remember to cache it so Spark won't keep reopening
   *  and reparsing it.
   */
  val wikiRdd: RDD[WikipediaArticle] = sc
                                         .textFile(WikipediaData.filePath)
                                         .map(WikipediaData.parse)
                                         .cache()

  /** Returns the number of articles on which the language `lang` occurs.
    *
    * We're told to consider `aggregate` on the RDD, although I don't see that as necessary for this.
    * We really just need to filter the RDD for stuff matching `lang`, and we're given a method
    * `mentionsLanguage()` to act as a filter.  After that it is simply counting the articles that
    * made it out of the filter.  (`count()` returns a Long so we need to convert it down.)
    *
    * The case of `lang` matters.  We are not converting its case, nor the text of the Wiki articles.
    * So "Java" and "java" will not match and will be counted separately.
    */
  def occurrencesOfLang(lang: String, rdd: RDD[WikipediaArticle]): Int =
    rdd.filter(_.mentionsLanguage(lang))
       .count()
       .toInt

  /* (1) Use `occurrencesOfLang` to compute the ranking of the languages
   *     (`val langs`) by determining the number of Wikipedia articles that
   *     mention each language at least once. Don't forget to sort the
   *     languages by their occurrence, in decreasing order!
   *
   *   Note: this operation is long-running. It can potentially run for
   *   several seconds.
   *
   *   Approach:  for every `lang` run `occurrencesOfLang` on the RDD, transforming the input into a list of
   *   pairs as we go.  "For every" means `map` or `flatMap`.  To sort there are a few options:
   *   - use sortBy(_._2).reverse:  this processes the list twice, which isn't necessary
   *   - use sortBy(- _._2):  notice the minus sign, to negate the value.  Because the second value of the pair
   *   is a number, we can use a math trick to get the normal sortBy()'s ascending order to reverse.  If a > b,
   *   then -a < -b and in an ascending sort -a will come before -b.  Thanks to immutability though, we aren't
   *   actually changing the value so the largest value ends up at the head of the list.  This isn't terribly
   *   clear, although it might perform well.
   *   - use sortWith(_._2 >= _._2):  this makes the descending sort on the 2nd part of the pair obvious.
   *
   *   Well, surprise surprise.  sortBy(- _._2) came in at 70 ms average and sortWith() 62.375 ms average.  I have
   *   a feeling the compiler or Spark was able to do something with the expression that it couldn't do for negating
   *   numbers.
   *
   *   I note that `rankLangs` is written just like it would look if the wiki articles were in a Seq or List instead
   *   of an RDD.  And that is pretty darn neat.
   *
   *   @return A list of pairs, consisting of the language and number of occurrences, in descending order.
   *   For example, List( ("Scala", 999999), ("JavaScript", 1278), ("Python", 982), ("Java", 42) )
   */
  def rankLangs(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] =
    langs.map( {lang: String => (lang, occurrencesOfLang(lang, rdd)) } )
         .sortWith(_._2 >= _._2)

  /* Compute an inverted index of the set of articles, mapping each language
   * to the Wikipedia pages in which it occurs.
   *
   * Approach:  We need to build an RDD of pairs from an RDD of articles.  That means we'll be mapping or
   * flatMapping over the RDD this time.
   *
   * For every article, filter the `langs` list to the langs that it mentions.  Since there can be multiple
   * languages in an article, we'll need to use `flatMap` instead of `map`.  At this point we have a list of
   * `langs` for the `article`, so now map over the filtered `langs` to turn it into individual pairs of
   * `(lang, article)`.  This results in `RDD[(lang, article)]` and we need to collect all the articles
   * associated with each `lang`.  That's exactly what `groupByKey()` does, so use it to create
   * `RDD[(lang, articles)]`, where `articles` is some iterable collection class (we don't care which).
   * Running `groupByKey()` will do an implicit `collect()` on the RDD so that we end up with an RDD that
   * looks like a `Map` object of key-value pairs, where the value is a collection of articles.
   */
  def makeIndex(langs: List[String], rdd: RDD[WikipediaArticle]): RDD[(String, Iterable[WikipediaArticle])] =
    rdd.flatMap( {article: WikipediaArticle => article.mentionsLangs(langs)
                                                      .map( {lang: String => (lang, article)} )
               } )
      .groupByKey()


  /* (2) Compute the language ranking again, but now using the inverted index. Can you notice
   *     a performance improvement?
   *
   *   Note: this operation is long-running. It can potentially run for
   *   several seconds.
   *
   *   Approach:  We have an RDD with each `lang` associated with all the `articles` that mention it.
   *   We need pairs of `lang` and occurrences.  This reminds me of a homework from a prior class, where
   *   we built a container of containers, and needed a container of counts.  So do the same thing here,
   *   the occurrences is simply the size of the value in the RDD key-value pair.  Therefore, map over
   *   the RDD values, replacing the `articles` with the size of their container.  Then `collect()`
   *   everything back, sort it in descending order and make it become a List.  After `makeIndex()`,
   *   this is pretty straightforward.
   */
  def rankLangsUsingIndex(index: RDD[(String, Iterable[WikipediaArticle])]): List[(String, Int)] =
    index.mapValues(_.size)
         .collect()
         .sortWith(_._2 >= _._2)
         .toList


  /* (3) Use `reduceByKey` so that the computation of the index and the ranking are combined.
   *     Can you notice an improvement in performance compared to measuring *both* the computation of the index
   *     and the computation of the ranking? If so, can you think of a reason?
   *
   *   Note: this operation is long-running. It can potentially run for
   *   several seconds.
   *
   *   Approach:  The first video in this week's lecture includes using `reduceByKey()` in a word counting
   *   example.  We follow that same kind of pattern.  The instructions above say to combine the indexing and
   *   the counting, which suggests starting with the guts of `makeIndex()`.  But instead of putting the
   *   actual article into a new RDD, we instead simply put a 1 in there.  We'll then use `reduceByKey()`
   *   to sum them all up (as in the classroom video).  We're returning a List instead of an RDD, so that
   *   might require a `collect()` call unless `reduceByKey()` happens to perform it under the covers.  That's
   *   followed by a descending sort on the 2nd value in each pair and converting the result to a list.
   */
  def rankLangsReduceByKey(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] =
    rdd.flatMap( _.mentionsLangs(langs).map( {lang: String => (lang, 1)} ) )   // NOTICE THE '1'!
       .reduceByKey(_ + _)
       .collect()
       .sortWith(_._2 >= _._2)
       .toList

  def main(args: Array[String]) {

    /* Languages ranked according to (1) */
    val langsRanked: List[(String, Int)] = timed("Part 1: naive ranking", rankLangs(langs, wikiRdd))

    /* An inverted index mapping languages to wikipedia pages on which they appear */
    def index: RDD[(String, Iterable[WikipediaArticle])] = makeIndex(langs, wikiRdd)

    /* Languages ranked according to (2), using the inverted index */
    val langsRanked2: List[(String, Int)] = timed("Part 2: ranking using inverted index", rankLangsUsingIndex(index))

    /* Languages ranked according to (3) */
    val langsRanked3: List[(String, Int)] = timed("Part 3: ranking using reduceByKey", rankLangsReduceByKey(langs, wikiRdd))

    /* Output the speed of each ranking */
    println(timing)
    sc.stop()
  }

  val timing = new StringBuffer
  def timed[T](label: String, code: => T): T = {
    val start = System.currentTimeMillis()
    val result = code
    val stop = System.currentTimeMillis()
    timing.append(s"Processing $label took ${stop - start} ms.\n")
    result
  }
}
