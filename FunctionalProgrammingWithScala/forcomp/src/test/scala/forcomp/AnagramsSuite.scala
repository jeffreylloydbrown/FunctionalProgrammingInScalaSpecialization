package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  /**
   * String.toList returns a list of characters.  Need to split by words, so use the regex
   * version of split() to look for non-word characters.  If there are multiple non-word
   * characters in a row in the string, split() will return empty list entries for them.
   * So filter the split() result for non-empty entries.
   */
  def makeSentence(s: String): Sentence = s.split("\\W").filter(e => e.nonEmpty).toList

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("") === List() )
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List()) === List() )
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
    assert(sentenceOccurrences(makeSentence("A Bridge Too Far")) ===
             List(('a',2), ('b',1), ('d',1), ('e',1), ('f',1), ('g',1), ('i',1), ('o',2), ('r',2), ('t',1)) )
    assert(sentenceOccurrences(makeSentence("A Bridge Too Far")) ===
          sentenceOccurrences(makeSentence("too FAR a Bridge")) )
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) ===
             Some(Set("ate", "eat", "tea")))
  }

  test("word anagrams: married") {
    assert(wordAnagrams("") === List())
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: jimmy - my") {
    assert(subtract(wordOccurrences("jimmy"), wordOccurrences("my")) === List(('i',1), ('j',1), ('m',1)) )
  }

  test("subtract: assessment - assess") {
    assert(subtract(wordOccurrences("assessment"), wordOccurrences("assess")) ===
             List(('e',1), ('m',1), ('n',1), ('t',1)) )
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(Nil, Nil) === Nil)
    assert(subtract(Nil, lard) === Nil)
    assert(subtract(lard, Nil) === lard)
    assert(subtract(lard, r) === lad)
  }

  test("better combinations: []") {
    assert(better_combinations(Nil) === List(Nil))
  }

  test("better combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    println("starting with " + abba)
    println("--------------------")
    assert(better_combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: kokok") {
    val kokok = List(('k',3), ('o',2))
    val kokokcomb = List(
      List(),
      List(('k',1)),
      List(('k',2)),
      List(('k',3)),
      List(('o',1)),
      List(('o',2)),
      List(('k',1), ('o',1)),
      List(('k',1), ('o',2)),
      List(('k',2), ('o',1)),
      List(('k',2), ('o',2)),
      List(('k',3), ('o',1)),
      List(('k',3), ('o',2))
    )
    assert(combinations(kokok).toSet === kokokcomb.toSet)
  }

  test("sentence anagrams: []") {
    assert(sentenceAnagrams(List()) === List(Nil))
  }

  test("sentence anagrams: Yes Man") {
    val yesman = List( List("en", "as", "my"),
                        List("en", "my", "as"),
                        List("man", "yes"),
                        List("men", "say"),
                        List("as", "en", "my"),
                        List("as", "my", "en"),
                        List("sane", "my"),
                        List("Sean", "my"),
                        List("my", "en", "as"),
                        List("my", "as", "en"),
                        List("my", "sane"),
                        List("my", "Sean"),
                        List("say", "men"),
                        List("yes", "man")
                      )
    val result = sentenceAnagrams(List("Yes", "man"))
    assert(result.length === yesman.length)
    assert(result.toSet === yesman.toSet)
  }

  test("sentence anagrams: You Olive") {
    val youolive = List(
      List("I", "you", "love"),
      List("I", "love", "you"),
      List("Io", "Lev", "you"),
      List("Io", "you", "Lev"),
      List("Lev", "Io", "you"),
      List("Lev", "you", "Io"),
      List("you", "I", "love"),
      List("you", "Io", "Lev"),
      List("you", "Lev", "Io"),
      List("you", "love", "I"),
      List("you", "olive"),
      List("love", "I", "you"),
      List("love", "you", "I"),
      List("olive", "you")
      )
    val result = sentenceAnagrams(List("You", "Olive"))
    assert(result.length === youolive.length)
    assert(result.toSet === youolive.toSet)
  }

  test("sentence anagrams: heather") {
    val heather = List(
      List("a", "the", "her"),
      List("a", "her", "the"),
      List("et", "ah", "her"),
      List("et", "ha", "her"),
      List("et", "her", "ah"),
      List("et", "her", "ha"),
      List("he", "he", "art"),
      List("he", "he", "rat"),
      List("he", "he", "tar"),
      List("he", "re", "hat"),
      List("he", "at", "her"),
      List("he", "her", "at"),
      List("he", "hat", "re"),
      List("he", "art", "he"),
      List("he", "rat", "he"),
      List("he", "tar", "he"),
      List("he", "earth"),
      List("he", "hater"),
      List("he", "heart"),
      List("re", "he", "hat"),
      List("re", "ah", "the"),
      List("re", "ha", "the"),
      List("re", "the", "ah"),
      List("re", "the", "ha"),
      List("re", "hat", "he"),
      List("re", "heath"),
      List("at", "he", "her"),
      List("at", "her", "he"),
      List("ah", "et", "her"),
      List("ah", "re", "the"),
      List("ah", "the", "re"),
      List("ah", "her", "et"),
      List("ah", "ether"),
      List("ah", "there"),
      List("ah", "three"),
      List("ha", "et", "her"),
      List("ha", "re", "the"),
      List("ha", "the", "re"),
      List("ha", "her", "et"),
      List("ha", "ether"),
      List("ha", "there"),
      List("ha", "three"),
      List("the", "a", "her"),
      List("the", "ah", "re"),
      List("the", "ha", "re"),
      List("the", "re", "ah"),
      List("the", "re", "ha"),
      List("the", "her", "a"),
      List("the", "hare"),
      List("the", "hear"),
      List("the", "Hera"),
      List("the", "Rhea"),
      List("her", "a", "the"),
      List("her", "ah", "et"),
      List("her", "ha", "et"),
      List("her", "at", "he"),
      List("her", "he", "at"),
      List("her", "et", "ah"),
      List("her", "et", "ha"),
      List("her", "the", "a"),
      List("her", "hate"),
      List("her", "heat"),
      List("her", "Thea"),
      List("hat", "he", "re"),
      List("hat", "re", "he"),
      List("hat", "here"),
      List("art", "he", "he"),
      List("rat", "he", "he"),
      List("tar", "he", "he"),
      List("here", "hat"),
      List("heath", "re"),
      List("hate", "her"),
      List("heat", "her"),
      List("Thea", "her"),
      List("hare", "the"),
      List("hear", "the"),
      List("Hera", "the"),
      List("Rhea", "the"),
      List("ether", "ah"),
      List("ether", "ha"),
      List("there", "ah"),
      List("there", "ha"),
      List("three", "ah"),
      List("three", "ha"),
      List("heather"),
      List("earth", "he"),
      List("hater", "he"),
      List("heart", "he")
    )
    val result = sentenceAnagrams(List("heather"))
    assert(result.length === heather.length)
    assert(result.toSet === heather.toSet)
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    val result = sentenceAnagrams(sentence)
    assert(result.length === anas.length)
    assert(result.toSet === anas.toSet)
  }

  test("sentence anagrams: repeat each test runs in <2ms") {
    val yesman = sentenceAnagrams(List("Yes", "man"))
    val youolive = sentenceAnagrams(List("You", "Olive"))
    val heather = sentenceAnagrams(List("heather"))
    val linux = sentenceAnagrams(List("Linux", "rulez"))

    println("I don't know how to test for execution time.  Confirm this test takes < 2 ms for all")
  }

}
