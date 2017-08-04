package patmat

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite
  {

    trait TestTrees
      {
        val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
        val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
        val t3 = Fork(Leaf('c', 4), Leaf('f', 5), List('c', 'f'), 9)
      }

    test("weight of a larger tree")
    {
      new TestTrees
        {
          assert(weight(t1) === 5)
          assert(weight(t2) === 9)
          assert(weight(makeCodeTree(t1, t2)) === 14)
        }
    }


    test("chars of a larger tree")
    {
      new TestTrees
        {
          assert(chars(t1) === List('a', 'b'))
          assert(chars(t2) === List('a', 'b', 'd'))
        }
    }


    test("string2chars(\"hello, world\")")
    {
      assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
    }

    test("times tests")
    {
      assert(times(string2Chars("")) === Nil)
      assert(times(Nil) === Nil)
      assert(times(string2Chars("a")) === List(('a', 1)))
      assert(times(string2Chars("aa")) === List(('a', 2)))
      assert(times(string2Chars("aaaaa")) === List(('a', 5)))

      val t = times(string2Chars("aba"))
      assert(t.sortWith(_._2 < _._2) === List(('b', 1), ('a', 2)))

      val x = times(List('a', 'b', 'a'))
      assert(x.sortWith(_._2 < _._2) === List(('b', 1), ('a', 2)))

      val y = times(string2Chars("abacabcadc"))
      assert(y.sortWith(_._2 < _._2) === List(('d', 1), ('b', 2), ('c', 3), ('a', 4)))
    }


    test("makeOrderedLeafList for some frequency table")
    {
      assert(makeOrderedLeafList(Nil) === Nil)
      assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
    }

    test("singleton tests")
    {
      new TestTrees
        {
          assert(singleton(Nil) === false)
          assert(singleton(List(t1)) === true)
          assert(singleton(List(makeCodeTree(t1, t2))) === true)
          assert(singleton(List(t1, t2)) === false)
          assert(singleton(List(Leaf('q', 2))) === true)
          assert(singleton(List(Leaf('q', 2), Leaf('r', 8))) === false)
        }
    }


    test("combine of some leaf list")
    {
      assert(combine(Nil) === Nil)

      val oneLeaf = List(Leaf('a', 47))
      assert(combine(oneLeaf) === oneLeaf)

      val leafList = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
      assert(combine(leafList) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))

      val lf = List(Leaf('e', 2), Leaf('t', 4), Leaf('x', 5))
      assert(combine(lf) === List(Leaf('x', 5), Fork(Leaf('e', 2), Leaf('t', 4), List('e', 't'), 6)))
    }

    test("until tests")
    {
      new TestTrees
        {
          assert(until(singleton, combine)(Nil) === Leaf('\u0000', 0))
          assert(until(singleton, combine)(List(t1)) === t1)

          val a = until(singleton, combine)(List(t1, t3))
          assert(a === Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5),
                            Fork(Leaf('c', 4), Leaf('f', 5), List('c', 'f'), 9),
                            List('a', 'b', 'c', 'f'),
                            14))
        }
    }

    test("createCodeTree")
    {
      new TestTrees
        {
          val jb = createCodeTree(string2Chars("Jeff Brown runs"))
          //println(jb)
        }
    }

    test("encode tests")
    {
      new TestTrees
        {
          assert(encode(t1)(Nil) === Nil)
          assert(encode(Leaf('b', 1))(List('a')) === Nil)
          assert(encode(Leaf('b', 1))(List('b')) === List(0))
          assert(encode(t1)(string2Chars("ab")) === List(0, 1))
          assert(encode(t1)(string2Chars("ba")) === List(1, 0))
          assert(encode(t1)(List('c')) === Nil) // 'c' isn't contained in the t1 CodeTree.
          assert(encode(makeCodeTree(t2, t3))(List('c')) === List(1, 0)) // 'c' is in t3, left leaf
          assert(encode(makeCodeTree(t2, t3))(List('b')) === List(0, 0, 1))
          assert(encode(makeCodeTree(t2, t3))(List('q')) === Nil) // 'q' not in CodeTree
        }
    }

    test("decode(t1, List(0,1,2)) has an illegal bit value")
    {
      intercept[java.lang.IllegalArgumentException]
        {
          new TestTrees
            {
              decode(t1, List(0,1,2))
            }
        }
    }

    test("decode tests")
    {
      new TestTrees
        {
          assert(decode(t1, Nil) === Nil)
          assert(decode(Leaf('a',12), List(0)) === List('a'))
          assert(decode(Leaf('a',12), List(1)) === Nil)
          assert(decode(t1, List(0,1)) === List('a','b'))
          assert(decode(t1, List(1,0)) === List('b','a'))
        }
    }

    test("convert tests")
    {
      new TestTrees
      {
        assert(convert(Leaf('a', 47)) === List( ('a',List(0)) ))
        assert(convert(t1) === List( ('a',List(0)), ('b',List(1)) ))
        assert(convert(t2) === List( ('a',List(0, 0)), ('b',List(0, 1)), ('d',List(1)) ))
        assert(convert(makeCodeTree(t1,t3)) === List( ('a',List(0, 0)), ('b',List(0, 1)),
                                                      ('c',List(1, 0)), ('f',List(1, 1)) ))
        assert(convert(makeCodeTree(t2,t3)) === List( ('a',List(0, 0, 0)), ('b',List(0, 0, 1)), ('d',List(0, 1)),
                                                      ('c',List(1, 0)), ('f',List(1, 1)) ))
      }
    }

    test("decode and encode a very short text should be identity")
    {
      new TestTrees
        {
          assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
        }
    }

    test("decode and quick-encode a very short text should be identity")
    {
      new TestTrees
        {
          assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
        }
    }

    test("decode the secret and show it")
    {
      println("the secret is: " + decodedSecret.mkString)
    }

  }
