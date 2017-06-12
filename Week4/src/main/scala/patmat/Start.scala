package patmat

import patmat.Huffman._
/**
  * Created by ayk on 11/06/2017.
  */
object Start {
  def main(args: Array[String]): Unit = {

    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

    val secret: List[Bit] = List(0, 1, 0, 0, 1)

    println(convert(t2))
    println(quickEncode(t2)(List('b', 'a', 'd')))

    println(decode(t2, secret).mkString)
    println(decodedSecret.mkString)


  }
}
