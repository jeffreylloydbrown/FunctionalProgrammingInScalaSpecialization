package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Leaf(_, w) => w
    case Fork(l, r, _, _) => weight(l) + weight(r)
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(c, _) => List(c)
    case Fork(l, r, _, _) => chars(l) ::: chars(r)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  /**
    * Algorithm: (previously I used span, which needed sorted input to work.  groupBy doesn't.
    * 1.  use groupBy to group by each character
    *         babcababc -> ( (a,a,a), (b,b,b,b), (c,c) )
    * 2.  use mapValues to replace each list value with its length.
    *         ( (a,a,a), (b,b,b,b), (c,c) ) -> ( (a,3), (b,4), (c,2) )
    *
    * Glad I finally found the 1 line implementation, and it doesn't involve mutable stuff.  Finally starting to
    * remember my list processing mindset.
    */
  def times(chars: List[Char]): List[(Char, Int)] = chars.groupBy(ch => ch).mapValues(_.length).toList

/*
  def times(chars: List[Char]): List[(Char, Int)] =
  {
    //println("times top: " + chars.toString())

    def timesAux (chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] =
      {
        //println("timesAux chars: " + chars.toString())
        //println("timesAux top acc: " + acc.toString())

        if (chars.isEmpty) acc
        else if (chars.head == acc.head._1)
          { // The first element has what we need, so replace it with
            // a new pair that has the count 1 higher, and stick
            // the accumulator tail onto it.  Then continue processing chars.
            val pair = (acc.head._1, acc.head._2 + 1)
            timesAux(chars.tail, List(pair) ++ acc.tail)
          }
        else
          { // need to search for chars.head in acc, while rebuilding acc recursively as a new list.
            // If not found, add (c, 1) to acc.  If found, increment the count by 1.
            def record (c: Char, prev: List[(Char, Int)], newAcc: List[(Char, Int)] ): List[(Char, Int)] =
              {
                //println("record c: " + c)
                //println("record prev: " + pref.toString())
                //println("record newAcc: " + newAcc.toString())

                if (prev.isEmpty) newAcc ++ List((c,1))   // not found case
                else if (c == prev.head._1)
                  { // found it, so increment and stop recursing
                    val pair = (prev.head._1, prev.head._2 + 1)
                    newAcc ++ List(pair) ++ prev.tail
                  }
                else record(c, prev.tail, newAcc ++ List(prev.head))    // keep searching
              }

            // we know the first element is not what we seek, so use it to
            // initialize the replacement accumulator we're about to search.
            timesAux(chars.tail, record(chars.head, acc.tail, List(acc.head) ))
          }
      }

    // On the initial call, there are no counts to search.  That means we know the first character has
    // a 1 count.  So initialize the accumulator with that information.
    if (chars.isEmpty) Nil
    else timesAux(chars.tail, List((chars.head, 1)))
  }
*/
  /**
    * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
    *
    * The returned list should be ordered by ascending weights (i.e. the
    * head of the list should have the smallest weight), where the weight
    * of a leaf is the frequency of the character.
    *
    * makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3))
    */
  def makeOrderedLeafList(frequencies: List[(Char, Int)]): List[Leaf] =
    frequencies.map(p => Leaf(p._1, p._2)).sortWith(_.weight < _.weight)

  /**
   * Checks whether the list `trees` contains only one single code tree.  This is true iff:
   * - trees itself isn't empty
   * - trees.tail is Empty
   *
   * because there is no "empty" version of Fork or Leaf, a CodeTree itself cannot be null.
   * That means we don't have to check for it or test for it.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees.nonEmpty && trees.tail.isEmpty

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   *
   * val leafList = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
   * assert(combine(leafList) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
   *
   * List( Leaf('e', 2), Leaf('t', 4), Leaf('x', 5) ) becomes
   * List( Leaf('x', 5), Fork(Leaf('e',2),Leaf('t',4),List('e', 't'),6) ) though.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] =
  {
    if (trees.isEmpty || singleton(trees)) trees        // 0 elements or 1 element returns trees unchanged.
    else
      {
        // insert() figures out where to put item into the rest of trees.  acc is the accumulator for the
        // new list we build.
        def insert (item: CodeTree, itemWeight: Int, trees: List[CodeTree], acc: List[CodeTree]) : List[CodeTree] =
        {
          if (trees.isEmpty)
            { // trees being empty means item needs to be last in the new list.
              acc ++ List(item)
            }
          else if (itemWeight <= weight(trees.head))
            { // the item goes here, sandwiched between acc and what's left of trees.
              acc ++ List(item) ++ trees
            }
          else
            { // continue searching, by moving trees.head to acc
              insert(item, itemWeight, trees.tail, acc ++ List(trees.head))
            }
        }

        // make a Fork of the first 2 nodes
        val f: CodeTree = makeCodeTree(trees.head, trees.tail.head)

        // pull out f's weight here so we do it once instead of on every recursion.
        insert(f, weight(f), trees.tail.tail, Nil)
      }
  }
  
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
    def until(singletonFunction: List[CodeTree] => Boolean,
              combineFunction: List[CodeTree] => List[CodeTree])
             (trees: List[CodeTree]): CodeTree =
    {
      if (trees.isEmpty) Leaf('\u0000', 0)      // this shouldn't happen, not ready to throw yet
      else if (singletonFunction(trees)) trees.head
      else until(singletonFunction, combineFunction)(combineFunction(trees))
    }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars)))


  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] =
  {
    def decodeAux (partial_tree: CodeTree, bits: List[Bit], chars: List[Char]): List[Char] =
    {
      partial_tree match {
        case Leaf(ch, _)      =>
          // Decoded a character.  Append it to the character list no matter what.  (Don't
          // precompute the new list cuz evaluation model makes sure only then or else runs.)
          // If no more bits, end the recursion.  If bits remain, continue processing but
          // start over at the top of the tree (from decode()) instead of moving down
          // partial_tree.  Be sure NOT to throw the current bit away, since we need it
          // at the top of the tree.
          if (bits.isEmpty) chars ++ List(ch)
          else decodeAux(tree, bits, chars ++ List(ch))

        case Fork(left, right, _, _) =>
          // We're inside the tree.  Bits isn't supposed to be empty yet, but if it is then
          // return the chars list and end the recursion.  (Or throw, don't know yet.)
          // Otherwise, use bits.head to decide on moving left or right.  Either way,
          // throw bits.head away since we've processed it.  We haven't found a character
          // yet so the chars list doesn't change here.
          if (bits.isEmpty) chars
          else decodeAux(if (bits.head == 0) left else right,
                         bits.tail, chars)
      }
    }

    // Perform our parameter checks.  decodeAux() will infinitely recurse if tree is just a Leaf() node, so
    // cover that degenerate case here.
    require(bits.forall(p => p == 0 || p == 1))  // Stupid I have to do this, Prof should have used enums not ints.
    if (bits.isEmpty) Nil
    else tree match {
      case Leaf(ch, _)      => if (bits.head == 0) List(ch) else Nil  // or just return List(ch) no matter what.
      case Fork(_, _, _, _) => decodeAux(tree, bits, Nil)
    }
  }
  
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode` Huffman tree defined above.
    **/
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)
  

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] =
  {
    // encodeAux does all the real work, processing over the entire text.  It has its
    // own helper routine that figures out the bits for a single character, while it
    // processes each character in the text.  There's probably a way to do this by
    // calling List::map() or similar, but I've already put a ton of time into this assignment.
    def encodeAux (text: List[Char], bits: List[Bit]): List[Bit] =
    {
      def encodeChar (tree: CodeTree, ch: Char, bits: List[Bit]): List[Bit] = tree match {
        case Fork(left, right, chars, _) =>
          if (!chars.contains(ch)) Nil    // short-circuit if ch isn't in this sub-CodeTree at all.
          else
            { val lb: List[Bit] = encodeChar(left, ch, bits ++ List(0))
              if (lb != Nil) lb
              else encodeChar(right, ch, bits ++ List(1))
            }
        case Leaf(char, _) => if (ch == char) bits
                              else Nil  // Came down the wrong path, so add no bits.
      }

      if (text.isEmpty) bits
      else encodeAux(text.tail, bits ++ encodeChar(tree, text.head, Nil))
    }

    // Check parameters.  tree cannot be empty, not even Nothing, because not even Nothing is able to represent an
    // empty tree.  If text isn't empty, handle the degenerate case of CodeTree being just a leaf node.
    // The general case, of it being a Fork, will get handled by encodeAux().
    if (text.isEmpty) Nil     // or throw new InvalidArgumentException("encode(tree)(text): 'text' cannot be empty")?
    else tree match {
      case Leaf(ch, _) => if (text.head == ch) List(0)
                          else Nil  // or throw new InvalidArgumentException("encode(tree)(text): 'text' has values not in 'tree'")?
      case Fork(_, _, _, _) => encodeAux(text, Nil)
      }
  }
  
  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.  Also sure there is a way to use a list searching method to do this in 1 line.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] =
    {
      if (table.isEmpty) Nil        // char wasn't in the CodeTable
      else if (table.head._1 == char) table.head._2     // found it, return the list of bits from the pair
      else codeBits(table.tail)(char)
    }
  
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable =
    {
      def convertAux (partial_tree: CodeTree, bits: List[Bit]): CodeTable = partial_tree match {
        case Leaf(ch, _)              => List( (ch, bits) )
        case Fork(left, right, _, _)  => mergeCodeTables(convertAux(left, bits ++ List(0)),
                                                         convertAux(right, bits ++ List(1)))
        }

      tree match {
        case Leaf(ch, _)              => List( (ch, List(0)) )
        case Fork(left, right, _, _)  => mergeCodeTables(convertAux(left, List(0)),
                                                         convertAux(right, List(1)))
      }
    }
  
  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ++ b
  
  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] =
  {
    def qeAux (table: CodeTable, text: List[Char], acc: List[Bit]): List[Bit] =
    {
      if (text.isEmpty) acc
      else qeAux(table, text.tail, acc ++ codeBits(table)(text.head))
    }

    qeAux(convert(tree), text, Nil)
  }
}
