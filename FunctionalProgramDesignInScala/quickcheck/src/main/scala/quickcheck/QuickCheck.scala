package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for { i <- arbitrary[Int]; h <- oneOf(const(empty), genHeap) } yield insert(i, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // We're given this property, and both Bogus1 and Bogus2 fail it.
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // Basic operation: add to an empty heap is no longer empty.
  property("add to empty -> not empty") = forAll { a: Int =>
    val h = insert(a, empty)
    ! isEmpty(h)
  }

  // Basic operation: the min of a 1-element heap is that element.
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // On a 2-element heap, insertion order doesn't matter and min for either is the same.
  property("hint 1:  min 2 values, order doesn't matter") = forAll { (a: Int, b: Int) =>
    val the_min = min(a,b)
    val h1 = insert(a, insert(b, empty))
    val h2 = insert(b, insert(a, empty))
    findMin(h1) == the_min && findMin(h2) == the_min
  }

  // Basic operation: empty + 1 - 1 = empty
  property("hint 2:  insert then deleteMin is empty") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  // Basic operation:  add a few, remove a few leaves empty.
  property("empty + 2 - 2 == empty") = forAll { (a: Int, b: Int) =>
    val has2 = insert(a, insert(b, empty))
    val has0 = deleteMin(deleteMin(has2))
    isEmpty(has0)
  }

  // Basic operation:  empty + 2 - 1 -> not empty
  property("add 2, remove 1 -> not empty") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    ! isEmpty(deleteMin(h))
  }

  // Hey, what if the values are the same.  It isn't a set, so shouldn't be empty.
  property("add same value twice, deleteMin -> not empty cuz its a queue") = forAll { a: Int =>
    val h = insert(a, insert(a, empty))
    ! isEmpty(deleteMin(h))
  }

  // For any heap, repeatedly finding a min and deleting it should naturally generate
  // a sorted list of results.  Bogus5 fails this.
  property("hint 3:  give me sorted results") = forAll { h: H =>
    def walk (h: H, prev: Int): Boolean = {
      if (isEmpty(h)) true
      else {
        val v = findMin(h)
        val new_h = deleteMin(h)
        prev <= v && walk(new_h, v)
      }
    }

    if (isEmpty(h)) true                  // handles 0-element heap
    else walk(deleteMin(h), findMin(h))   // 1-element heap is "sorted" by definition, so initing with findMin is OK
  }

  // Melding 2 heaps should return the minimum from one or the other of them.
  property("hint 4:  meld 2 heaps, should be min from 1 of original heaps") = forAll { (h1: H, h2: H) =>
    val the_min = findMin(meld(h1, h2))
    the_min == findMin(h1) || the_min == findMin(h2)
  }

  // Walk a heap generated from a list.  Sort the list, confirm the items coming off both match.
  // This case caught both Bogus3 and Bogus4.
  property("make heap from a list, walk both") = forAll { l: List[Int] =>
    // Given a sequence, build a heap from it.  foldLeft perfect for this, since we're building a new data structure
    // by traversing an existing one.  acc starts out with Heap::empty.
    def makeHeap (l: List[Int]): H = l.foldLeft(empty) ( (acc, elem) => insert(elem, acc) )

    // Walk the heap and the sorted list.  Each list element has to match each findMin, with deleteMin
    // making the heap smaller on the next recursion and list::tail making the list smaller.
    def walk(h: H, l: List[Int]): Boolean = {
      // if either goes empty, the other must also be empty
      if (isEmpty(h)) l.isEmpty
      else if (l.isEmpty) isEmpty(h)
      else
        // neither empty.  first items must match and recurse with rest.
        findMin(h) == l.head && walk(deleteMin(h), l.tail)
    }

    walk(makeHeap(l), l.sorted)
  }

}
