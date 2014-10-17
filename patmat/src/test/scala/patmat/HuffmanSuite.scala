package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("run easy scenario encoding/decoding") {
    val easyTest = "ababaz".toList
    val easyResult = times(easyTest)
    val orderedEasyResult = makeOrderedLeafList(easyResult)
    val easyCodeTree = createCodeTree(easyTest)
    
	val easyEncodedTest = encode(easyCodeTree)(easyTest)
	val easyDecodedTest = decode(easyCodeTree, easyEncodedTest).mkString
    assert(easyDecodedTest === "ababaz")
	
	val easyQuickEncoded = quickEncode(easyCodeTree)(easyTest)
	val easyQuickDecoded = decode(easyCodeTree, easyQuickEncoded).mkString
    assert(easyDecodedTest === easyQuickDecoded)
  }

  test("run med scenario encoding/decoding") {
    val medTest = "hello world".toList
    val medResult = times(medTest)
    val orderedMedResult = makeOrderedLeafList(medResult)
    val medCodeTree = createCodeTree(medTest)
    
	val medEncodedTest = encode(medCodeTree)(medTest)
	val medDecodedTest = decode(medCodeTree, medEncodedTest).mkString
    assert(medDecodedTest === "hello world")
	
	val medQuickEncoded = quickEncode(medCodeTree)(medTest)
	val medQuickDecoded = decode(medCodeTree, medQuickEncoded).mkString
    assert(medDecodedTest === medQuickDecoded)
  }

  test("run high scenario encoding/decoding") {
    val hardTest = "Because the Java application is run from within a Map-Reduce job".toList
    val hardCodeTree = createCodeTree(hardTest)
    val hardEncodedTest = encode(hardCodeTree)(hardTest)
    val hardDecodedTest = decode(hardCodeTree, hardEncodedTest).mkString
    assert(hardDecodedTest === "Because the Java application is run from within a Map-Reduce job")
    
    val hardQuickEncoded = quickEncode(hardCodeTree)(hardTest)
    val hardQuickDecoded = decode(hardCodeTree, hardQuickEncoded).mkString
    assert(hardDecodedTest === hardQuickDecoded)
  }
}
