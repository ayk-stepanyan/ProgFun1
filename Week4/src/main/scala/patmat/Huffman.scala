package patmat

import common._

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

  def weight(tree: CodeTree): Int = tree match {
    case Leaf(_, weight) => weight
    case Fork(_, _, _, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(char, _) => char :: Nil
    case Fork(_, _, chars, _) => chars
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {

    def charTimes(char: Char, chars: List[Char]): Int = chars match {
      case List() => 0
      case List(c) => if (c == char) 1 else 0
      case c :: cs => if (c == char) 1 + charTimes(char, cs) else charTimes(char, cs)
    }

    chars match {
      case List() => Nil
      case List(c) => (c, 1) :: Nil
      case c :: cs => (c, 1 + charTimes(c, cs)) :: times(cs.filter(_ != c))
    }
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs.sortBy(_._2).map { case (char, int) => Leaf(char, int) }
  }

  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case Nil => false
    case x :: Nil => true
    case x :: xs => false
  }

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case Nil => trees
    case ct :: Nil => trees
    case ct1 :: ct2 :: c => (makeCodeTree(ct1, ct2) :: c).sortBy(weight)
  }

  def until(predicate: List[CodeTree] => Boolean, iteration: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): CodeTree = {
    if (predicate(trees)) trees.head
    else until(predicate, iteration)(iteration(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars)))

  type Bit = Int

  /**
    * This function decodes the bit sequence `bits` using the code tree `tree` and returns
    * the resulting list of characters.
    */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decodeChar(currTree: CodeTree, currBits: List[Bit]): List[Char] = (currTree, currBits) match {
      case (Leaf(char, _), cb) => char :: decodeChar(tree, cb)
      case (_, Nil) => Nil
      case (Fork(left, right, _, _), b :: bs) => decodeChar(if (b == 0) left else right, bs)
    }

    decodeChar(tree, bits)
  }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)

  // Part 4a: Encoding using Huffman tree

  /**
    * This function encodes `text` using the code tree `tree`
    * into a sequence of bits.
    */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeChar(currTree: CodeTree, currText: List[Char]): List[Bit] = (currTree, currText) match {
      case (_, Nil) => Nil
      case (Leaf(_, _), b :: bs) => encodeChar(tree, bs)
      case (Fork(left, right, _, _), b :: bs) => if (chars(left).contains(b)) 0 :: encodeChar(left, b :: bs) else 1 :: encodeChar(right, b :: bs)
    }

    encodeChar(tree, text)
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
    * This function returns the bit sequence that represents the character `char` in
    * the code table `table`.
    */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table.find { case (cc, b) => cc == char }.head._2

  /**
    * Given a code tree, create a code table which contains, for every character in the
    * code tree, the sequence of bits representing that character.
    *
    * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
    * a valid code tree that can be represented as a code table. Using the code tables of the
    * sub-trees, think of how to build the code table for the entire tree.
    */
  def convert(tree: CodeTree): CodeTable = tree match {
    case Leaf(char, _) => (char, Nil) :: Nil
    case Fork(left, right, _, _) => mergeCodeTables(convert(left).map { case (cc, b) => (cc, 0 :: b) }, convert(right).map { case (cc, b) => (cc, 1 :: b) })
  }

  /**
    * This function takes two code tables and merges them into one. Depending on how you
    * use it in the `convert` method above, this merge method might also do some transformations
    * on the two parameter code tables.
    */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    a ::: b
  }

  /**
    * This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to a code table
    * and then uses it to perform the actual encoding.
    */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def quickEncodeStep(codeTable: CodeTable, currText: List[Char]): List[Bit] = {
      if (currText.isEmpty) Nil
      else codeBits(codeTable)(currText.head):::quickEncodeStep(codeTable, currText.tail)
    }

    quickEncodeStep(convert(tree), text)
  }
}
