package forcomp

object AnagramMap {
  private var anagramMap = Map[List[(Char, Int)], List[List[String]]]()
  def get(k: List[(Char, Int)]): List[List[String]] = anagramMap.getOrElse(k, Nil)
  def put(k: List[(Char, Int)], v: List[List[String]]) = { anagramMap = anagramMap.updated(k, v) }
}

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences =
    w.toLowerCase().toList.groupBy(ch => ch).mapValues(_.length).toList.sorted

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences)

  /** Returns all the anagrams of a given word.  If the word isn't in the dictionary, return Nil. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word), Nil)

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] =
  {
    // This command will return the combination list, but it will include multiple entries with the same
    // letter key.  Those entries have to get filtered out.
    //
    // Step 1: Turn List( ('a', 2), ('b', 2) ) -> List( ('a',1), ('a',2), ('b',1), ('b',2) )
    val step1: List[(Char, Int)] = for {(ch, count) <- occurrences
                                        j <- 1 to count
                                       } yield (ch, j)

    // Step 2: use Set::subsets to generate all the subsets.  Empty is included (good), but so are
    // subsets that have multiple copies of the same character:  ('a', 1), ('a', 2)
    // List(
    // List(),
    // List((a,1)),
    // List((a,2)),
    // List((b,1)),
    // List((b,2)),
    // List((a,1), (a,2)),            filter out
    // List((a,1), (b,1)),
    // List((a,1), (b,2)),
    // List((a,2), (b,1)),
    // List((a,2), (b,2)),
    // List((b,1), (b,2)),            filter out
    // List((a,1), (a,2), (b,1)),     filter out
    // List((a,1), (a,2), (b,2)),     filter out
    // List((a,1), (b,1), (b,2)),     filter out
    // List((a,2), (b,1), (b,2)),     filter out
    // List((a,1), (a,2), (b,1), (b,2)))  filter out
    val step2 = step1.toSet.subsets.map(_.toList).toList

    // Step 3:  filter out lists with duplicate characters.
    //    Step 3a: group the list by each character.
    //    Step 3b: for each item (forall), keep it if the associated list is <= length 1.  Length 2
    //             or longer means the character occurred more than once.  Length 0 means the list is empty.
    //    Step 3c: each list member isn't guaranteed to be sorted as an occurrences list.  Use map to sort each one.
    val step3 = step2.filter( _.groupBy(_._1).forall(_._2.length <= 1) ).map(_.sorted)

    // step3 is the answer!
    step3
  }

  // Using foldRight processes the occurrence list right to left, which keeps each list within the combination
  // set in order by letter without sorting.  Using foldLeft puts the occurrence list into backward order, and you have
  // to then map a sort call over each element to fix it.  Figuring out the right starting value for the accumulator
  // is important; starting with just Nil means there is nothing for the :: operator to add to in the for
  // comprehension.  So List[Occurrences](Nil) fixes that.
  //
  // The breakthru realization is that in this case the "accumulator" is really an accumulator of accumulators, with
  // each fold step adding to each accumulator in that listing.  This unfortunately wasn't immediately obvious
  // to me reading the original comments in the homework.  I didn't fully visualize the recursive nature of the
  // combination list.  But now I have a better tool for similar future problems :-)
  //
  // The second breakthru is that foldLeft/foldRight doesn't have to reduce the original container to single values.
  // It returns the accumulator, and the accumulator can be ANYTHING...in this case a completely different data type.
  // Will be obvious to the experienced scala person, but I'm still a neophyte.
  def better_combinations (occurrences: Occurrences): List[Occurrences] =
    (occurrences.foldRight(List[Occurrences](Nil))) {
      case ( (ch, cnt), acc ) => {
                                println("acc starts with " + acc)
                                println("char = " + ch + ", count = " + cnt)
                                val justfound = (for {comb <- acc; n <- 1 to cnt}
                                                 yield {
                                                   println("acc = " + acc)
                                                   println("comb = " + comb)
                                                   println("adding " + (ch,n) + " to " + comb)
                                                   println("--------------------")
                                                   (ch, n) :: comb
                                                 }
                                                )
                                println("for comp result: " + justfound)
                                val newacc = acc ++ justfound
                                println("acc now is " + newacc)
                                println("===================")
                                newacc
                              }
    }

  /** Subtracts occurrence list `y` from occurrence list `x`.
    *
    *  The precondition is that the occurrence list `y` is a subset of
    *  the occurrence list `x` -- any character appearing in `y` must
    *  appear in `x`, and its frequency in `y` must be smaller or equal
    *  than its frequency in `x`.
    *
    *  Note: the resulting value is an occurrence - meaning it is sorted
    *  and has no zero-entries.
    *
    *  Algorithm: x.diff(y) is not correct because it doesn't handle the counts
    *  on each pair.  So:
    *  for each pair in y
    *    if _1 not in x throw Error "precondition violated"
    *    else replace (x._1, x._2) with (x._1, x._2 - y._2)
    *  filter that transformation with filter(_._2 > 0) and sort it to be sure.
    *  I'm guessing that either foldleft() or foldright() replaces most of this but I'm not figuring that out.
    */

  def subtract(x: Occurrences, y: Occurrences): Occurrences =
    if (x.isEmpty) Nil    // if x == Nil avoids calls to subtractAux, filter and sorted for no reason.
    else
    {
      def subtractAux (x: Occurrences, y: Occurrences): Occurrences = (x, y) match
      {
        //case (Nil, _)                  => Nil   cannot happen
        case (_, Nil)                  => x
        case (xs, (ych, ycount) :: ys) =>
          // use map() to look at each pair.  If the letter matches ych, subtract ycount from it.  Otherwise no change.
          subtractAux(xs.map(p => ( p._1, p._2 - (if (p._1 == ych) ycount else 0) ) ), ys)
      }

      subtractAux(x, y).filter(_._2 > 0).sorted
    }

  // This "better" subtract relies on a data structure that can find the correct item and update that same item.
  // List can do this, but an Occurrence list is a list of pairs.  And finding/replacing the pair is a pain because
  // the find has to be "by character only" since I don't know the character count.  Since I'm finding by a key only,
  // might as well use the data structure meant to find by keys...a map.  And map has easy item removal, whereas
  // List needs appropriate drop and take calls.
  //
  // foldLeft takes a starting point.  For us that is the `x` list.  It will then call our operation with the
  // accumulator (which started as the `x` list) and each pair in `y`, (ych, ycnt).  So the operation is to look for
  // ych in the accumulator.  If found and the new count is > 0, update the count with the new value.  Otherwise,
  // remove `ych` from the accumulator.
  //
  // The result is a modified accumulator.  Since we started with `x`, the result is a modified `x`.  It won't be
  // sorted, so we need to sort by each character to make it into an occurrence list again.
  def better_subtract (x: Occurrences, y: Occurrences): Occurrences = {
    type Pair = (Char, Int)

    def subtract_pair (map: Map[Char,Int], y_elem: Pair ): Map[Char,Int] = {
      val ych: Char = y_elem._1
      val ycnt: Int = y_elem._2
      val new_count: Int = map.getOrElse(ych,0) - ycnt
      if (new_count > 0) map.updated(ych, new_count) else map - ych
    }

    y.foldLeft(x.toMap)(subtract_pair).toList.sorted
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   *
   *  Note: A "sentence" qualifies only if it uses all letters of the original sentence.  Notice that each case above
   *  has a total of 6 letters in each sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] =
  {
    if (sentence.isEmpty) List(Nil)
    else
    {
      def attemptSentence(st: List[Word], rest: Occurrences): List[Sentence] =
      {
        val lookup: List[Sentence] = AnagramMap.get(rest)
        if (rest.isEmpty) List(st)
        else if (lookup.nonEmpty) lookup
        else
        {
          // for each occurrence in the list of combinations, look for the word or words that match it.
          // remember, dictionaryByOccurrences returns a List[Word], so must process each one.
          // If word is in dictionary, then continue attempting this sentence by adding the word to our
          // sentence and subtracting the occurrence list from what we started with.  Any failures will put
          // List() in for the failed sentence, and we will filter those out in the main routine.
          val addon: List[List[Sentence]] = for {o: Occurrences <- combinations(rest)
                                                 wd: Word <- dictionaryByOccurrences.getOrElse(o, List(""))
                                                 if wd != ""}
            yield attemptSentence(st ++ List(wd), subtract(rest, o))

          // addon is a List of List of Sentence.  We need to flatten it one level to return it.
          // addon will not ever be empty (we return List(word) at the very minimum, above).
          addon.flatten
        }
      }

      // Call attemptSentence() to do almost all the work recursively.  The answer it hands back will contain
      // Nil entries for failed tries, so we need to filter those out.  attemptSentence() will never return just
      // Nil, so we don't have to worry about that.  Also, we checked above for an empty sentence and returned an
      // empty anagram, so we don't have to put 1 Nil entry back into the result here.
      val occurrences = sentenceOccurrences(sentence)
      val sentences: List[Sentence] = attemptSentence(Nil, occurrences).filter(_ != Nil)
      AnagramMap.put(occurrences, sentences)
      sentences
    }
  }

/* NOT MY CODE
    // Found this version online after I finished mine.  It is prettier, and shows I didn't need to test stuff
    // like I thought.  I didn't think about doing the recursion in the for loop itself instead of the yield.  That
    // avoids the List of List of Sentence problem.  For all but Linux Rulez it is essentially the same performance,
    // and for Linux Rulez this is only marginally faster.  So for my first foray into the language, not bad!
    //
    // before trying the map lookup optimization:             average without high or low result
    // Yes Man:       31ms, 31ms, 31ms, 31ms, 31ms, 31ms  ->  31ms
    // You Olive:     16ms, 31ms, 31ms, 31ms, 31ms, 31ms  ->  31ms
    // heather:       16ms, 31ms, 16ms, 16ms, 16ms, 31ms  ->  19.75ms
    // Linux rulez:   62ms, 78ms, 63ms, 78ms, 78ms, 63ms  ->  70.5ms
    //
    // with map lookup optimization:                          average without high or low result
    // Yes Man:       47ms,  32ms, 47ms, 40ms, 31ms, 30ms ->  37.5ms
    // You Olive:     15ms,  31ms, 16ms, 20ms, 32ms, 20ms ->  21.75ms
    // heather:       31ms,  15ms, 15ms, 20ms, 15ms, 20ms ->  17.5ms
    // Linux rulez:   78ms, 130ms, 78ms, 90ms, 78ms, 80ms ->  81.5ms
    //
    // You Olive benefits a little from Yes Man having already run.  Same for heather ("a" is mapped).
    // But rerunning all of them again takes <1ms total, for all 4 combined, all of them.  See test 18.
    //

    def sentenceAnagrams(sentence: Sentence): List[Sentence] =
    {
      def iter (occurrences: Occurrences): List[Sentence] =
      {
        val lookup: List[Sentence] = AnagramMap.get(occurrences)
        if (occurrences.isEmpty) List(Nil)
        else if (lookup.nonEmpty) lookup
        else
        {
          val sentences: List[Sentence] =
            for {
                  combination <- combinations(occurrences)
                  word <- dictionaryByOccurrences.getOrElse(combination, Nil)
                  sentence <- iter(subtract(occurrences, wordOccurrences(word)))
                  if combination.nonEmpty
                } yield word :: sentence
          AnagramMap.put(occurrences, sentences)
          sentences
        }
      }

      iter (sentenceOccurrences(sentence))
    }
  NOT MY CODE */
}
