package patmat

import common._

object HuffmanTest {
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(350); 

  def weight(tree: CodeTree): Int = tree match {
    case Fork(l, r, cs, w) => w
    case Leaf(c, w) => w
  };System.out.println("""weight: (tree: patmat.HuffmanTest.CodeTree)Int""");$skip(124); 

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(l, r, cs, w) => cs
    case Leaf(c, w) => List(c)
  };System.out.println("""chars: (tree: patmat.HuffmanTest.CodeTree)List[Char]""");$skip(137); 

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right));System.out.println("""makeCodeTree: (left: patmat.HuffmanTest.CodeTree, right: patmat.HuffmanTest.CodeTree)patmat.HuffmanTest.Fork""");$skip(220); 
  
  def insert[T](comparator: (T, T) => Boolean)(x: T, xs: List[T]): List[T] = xs match {
    case List() => List(x)
    case y :: ys =>
      if (comparator(x, y)) x :: xs
      else y :: insert(comparator)(x, ys)
  };System.out.println("""insert: [T](comparator: (T, T) => Boolean)(x: T, xs: List[T])List[T]""");$skip(203); 
	
  def count(c: Char, acc: List[(Char, Int)]): List[(Char, Int)] = acc match {
    case Nil => List((c, 1))
    case y :: ys =>
      if (c == y._1) (c, y._2 + 1) :: ys
      else y :: count(c, ys)
  };System.out.println("""count: (c: Char, acc: List[(Char, Int)])List[(Char, Int)]""");$skip(136); 
  
  def times(chars: List[Char]): List[(Char, Int)] = chars match {
    case Nil => List()
    case x :: xs => count(x, times(xs))
  };System.out.println("""times: (chars: List[Char])List[(Char, Int)]""");$skip(55); val res$0 = 

	times(List('a','a','z','z','z','z','a','b','c','b'));System.out.println("""res0: List[(Char, Int)] = """ + $show(res$0));$skip(74); 

	def leafListInsert = insert[Leaf]((l1, l2) => l1.weight <= l2.weight) _;System.out.println("""leafListInsert: => (patmat.HuffmanTest.Leaf, List[patmat.HuffmanTest.Leaf]) => List[patmat.HuffmanTest.Leaf]""");$skip(187); 
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs match {
  	case Nil => List()
  	case y :: ys =>
  		leafListInsert(Leaf(y._1, y._2), makeOrderedLeafList(ys))
  };System.out.println("""makeOrderedLeafList: (freqs: List[(Char, Int)])List[patmat.HuffmanTest.Leaf]""");$skip(160); 
                                              
	def singleton(trees: List[CodeTree]): Boolean = trees match {
    case y :: Nil => true
    case _ => false
  };System.out.println("""singleton: (trees: List[patmat.HuffmanTest.CodeTree])Boolean""");$skip(80); 

	def codeTreeInsert = insert[CodeTree]((t1, t2) => weight(t1) <= weight(t2)) _;System.out.println("""codeTreeInsert: => (patmat.HuffmanTest.CodeTree, List[patmat.HuffmanTest.CodeTree]) => List[patmat.HuffmanTest.CodeTree]""");$skip(190); 
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case Nil => Nil
    case x :: y :: rest => codeTreeInsert(makeCodeTree(x, y), rest)
    case y :: Nil => List(y)
  };System.out.println("""combine: (trees: List[patmat.HuffmanTest.CodeTree])List[patmat.HuffmanTest.CodeTree]""");$skip(64); 

	val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4));System.out.println("""leaflist  : List[patmat.HuffmanTest.Leaf] = """ + $show(leaflist ));$skip(34); 
	val combined = combine(leaflist);System.out.println("""combined  : List[patmat.HuffmanTest.CodeTree] = """ + $show(combined ));$skip(209); 
	//val finalCombined = combine(combined)
    
  def until(t: List[CodeTree] => Boolean, c: List[CodeTree] => List[CodeTree])(ts: List[CodeTree]): List[CodeTree] =
    if (t(ts)) ts
    else until(t, c)(c(ts));System.out.println("""until: (t: List[patmat.HuffmanTest.CodeTree] => Boolean, c: List[patmat.HuffmanTest.CodeTree] => List[patmat.HuffmanTest.CodeTree])(ts: List[patmat.HuffmanTest.CodeTree])List[patmat.HuffmanTest.CodeTree]""");$skip(196); 

  def createCodeTree(chars: List[Char]): CodeTree = chars match {
    case List(x, _*) =>
      until(singleton, combine)(makeOrderedLeafList(times(chars))).head
    case Nil => Leaf('0', 0)
  };System.out.println("""createCodeTree: (chars: List[Char])patmat.HuffmanTest.CodeTree""");$skip(412); 
  
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def step(subtree: CodeTree, xs: List[Bit], acc: List[Char]): List[Char] = subtree match {
      case Fork(l, r, cs, w) => step(if (xs.head == 0) l else r, xs.tail, acc)
      case Leaf(c, w) => {
      	val next = acc ::: List(c)
      	if (xs.isEmpty) next
      	else step(tree, xs, next)
      }
    }
    
    step(tree, bits, List())
  };System.out.println("""decode: (tree: patmat.HuffmanTest.CodeTree, bits: List[patmat.HuffmanTest.Bit])List[Char]""");$skip(160); 
  
  def contains(char: Char, chars: List[Char]): Boolean =
  	if (chars.isEmpty) false
  	else if (chars.head == char) true
  	else contains(char, chars.tail);System.out.println("""contains: (char: Char, chars: List[Char])Boolean""");$skip(459); 
  	
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def step(subtree: CodeTree, xs: List[Char], acc: List[Bit]): List[Bit] = subtree match {
      case Fork(l, r, cs, w) =>
      	if (contains(xs.head, chars(l))) step(l, xs, acc ::: List(0))
      	else step(r, xs, acc ::: List(1))
      case Leaf(c, w) => xs match {
      	case y :: Nil => acc
      	case _ => step(tree, xs.tail, acc)
      }
    }
    
    step(tree, text, List())
  };System.out.println("""encode: (tree: patmat.HuffmanTest.CodeTree)(text: List[Char])List[patmat.HuffmanTest.Bit]""");$skip(36); 
  
  val easyTest = "ababaz".toList;System.out.println("""easyTest  : List[Char] = """ + $show(easyTest ));$skip(35); 
  val easyResult = times(easyTest);System.out.println("""easyResult  : List[(Char, Int)] = """ + $show(easyResult ));$skip(58); 
  val orderedEasyResult = makeOrderedLeafList(easyResult);System.out.println("""orderedEasyResult  : List[patmat.HuffmanTest.Leaf] = """ + $show(orderedEasyResult ));$skip(46); 
  val easyCodeTree = createCodeTree(easyTest);System.out.println("""easyCodeTree  : patmat.HuffmanTest.CodeTree = """ + $show(easyCodeTree ));$skip(40); 
  
  val medTest = "hello world".toList;System.out.println("""medTest  : List[Char] = """ + $show(medTest ));$skip(33); 
  val medResult = times(medTest);System.out.println("""medResult  : List[(Char, Int)] = """ + $show(medResult ));$skip(56); 
  val orderedMedResult = makeOrderedLeafList(medResult);System.out.println("""orderedMedResult  : List[patmat.HuffmanTest.Leaf] = """ + $show(orderedMedResult ));$skip(44); 
  val medCodeTree = createCodeTree(medTest);System.out.println("""medCodeTree  : patmat.HuffmanTest.CodeTree = """ + $show(medCodeTree ));$skip(48); 

	val exceptionTest = createCodeTree("".toList);System.out.println("""exceptionTest  : patmat.HuffmanTest.CodeTree = """ + $show(exceptionTest ));$skip(54); 
	val easyEncodedTest = encode(easyCodeTree)(easyTest);System.out.println("""easyEncodedTest  : List[patmat.HuffmanTest.Bit] = """ + $show(easyEncodedTest ));$skip(70); 
	val easyDecodedTest = decode(easyCodeTree, easyEncodedTest).mkString;System.out.println("""easyDecodedTest  : String = """ + $show(easyDecodedTest ));$skip(53); 
	
	val medEncodedTest = encode(medCodeTree)(medTest);System.out.println("""medEncodedTest  : List[patmat.HuffmanTest.Bit] = """ + $show(medEncodedTest ));$skip(67); 
	val medDecodedTest = decode(medCodeTree, medEncodedTest).mkString;System.out.println("""medDecodedTest  : String = """ + $show(medDecodedTest ));$skip(92); 

  val hardTest = "Because the Java application is run from within a Map-Reduce job".toList;System.out.println("""hardTest  : List[Char] = """ + $show(hardTest ));$skip(46); 
  val hardCodeTree = createCodeTree(hardTest);System.out.println("""hardCodeTree  : patmat.HuffmanTest.CodeTree = """ + $show(hardCodeTree ));$skip(55); 
  val hardEncodedTest = encode(hardCodeTree)(hardTest);System.out.println("""hardEncodedTest  : List[patmat.HuffmanTest.Bit] = """ + $show(hardEncodedTest ));$skip(71); 
  val hardDecodedTest = decode(hardCodeTree, hardEncodedTest).mkString
            
	type Bit = Int;System.out.println("""hardDecodedTest  : String = """ + $show(hardDecodedTest ));$skip(1527); 
  
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387);System.out.println("""frenchCode  : patmat.HuffmanTest.CodeTree = """ + $show(frenchCode ));$skip(158); 
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1);System.out.println("""secret  : List[patmat.HuffmanTest.Bit] = """ + $show(secret ));$skip(59); 

  val decodedSecret = decode(frenchCode, secret).mkString
  
  
  
  type CodeTable = List[(Char, List[Bit])];System.out.println("""decodedSecret  : String = """ + $show(decodedSecret ));$skip(198); 
  
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a match {
    case Nil => b
    case x :: xs => x :: mergeCodeTables(xs, b)
  };System.out.println("""mergeCodeTables: (a: patmat.HuffmanTest.CodeTable, b: patmat.HuffmanTest.CodeTable)patmat.HuffmanTest.CodeTable""");$skip(315); 
  
  def convert(tree: CodeTree): CodeTable = {
    def build(subTree: CodeTree, acc: List[Bit]): CodeTable = subTree match {
      case Fork(l, r, cs, w) =>
        mergeCodeTables(build(l, acc ::: List(0)), build(r, acc ::: List(1)))
      case Leaf(c, w) => List((c, acc))
    }
    
    build(tree, List())
  };System.out.println("""convert: (tree: patmat.HuffmanTest.CodeTree)patmat.HuffmanTest.CodeTable""");$skip(181); 
  
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case Nil => List()
    case x :: xs =>
      if (x._1 == char) x._2
      else codeBits(xs)(char)
  };System.out.println("""codeBits: (table: patmat.HuffmanTest.CodeTable)(char: Char)List[patmat.HuffmanTest.Bit]""");$skip(271); 
  
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def generate(t: CodeTable, xs: List[Char]): List[Bit] = xs match {
      case Nil => Nil
      case x :: xs1 => codeBits(t)(x) ::: generate(t, xs1)
    }
    
    generate(convert(tree), text)
  };System.out.println("""quickEncode: (tree: patmat.HuffmanTest.CodeTree)(text: List[Char])List[patmat.HuffmanTest.Bit]""");$skip(83); 
  
  val tableA = List(('a', List(0,0,1)), ('b', List(0,1,0)), ('c', List(0,1,1)));System.out.println("""tableA  : List[(Char, List[Int])] = """ + $show(tableA ));$skip(80); 
  val tableB = List(('d', List(1,0,0)), ('e', List(1,0,1)), ('f', List(1,1,1)));System.out.println("""tableB  : List[(Char, List[Int])] = """ + $show(tableB ));$skip(37); val res$1 = 
  
  mergeCodeTables(tableA, tableB);System.out.println("""res1: patmat.HuffmanTest.CodeTable = """ + $show(res$1));$skip(47); 
  
  val easyCodeTable = convert(easyCodeTree);System.out.println("""easyCodeTable  : patmat.HuffmanTest.CodeTable = """ + $show(easyCodeTable ));$skip(45); 
  
  val medCodeTable = convert(medCodeTree);System.out.println("""medCodeTable  : patmat.HuffmanTest.CodeTable = """ + $show(medCodeTable ));$skip(98); 
                                                   
  val bitsForA = codeBits(easyCodeTable)('a');System.out.println("""bitsForA  : List[patmat.HuffmanTest.Bit] = """ + $show(bitsForA ));$skip(45); 
  val bitsForH = codeBits(medCodeTable)('h');System.out.println("""bitsForH  : List[patmat.HuffmanTest.Bit] = """ + $show(bitsForH ));$skip(61); 
  val easyQuickEncoded = quickEncode(easyCodeTree)(easyTest);System.out.println("""easyQuickEncoded  : List[patmat.HuffmanTest.Bit] = """ + $show(easyQuickEncoded ));$skip(58); 
  val medQuickEncoded = quickEncode(medCodeTree)(medTest);System.out.println("""medQuickEncoded  : List[patmat.HuffmanTest.Bit] = """ + $show(medQuickEncoded ));$skip(61); 
  val hardQuickEncoded = quickEncode(hardCodeTree)(hardTest);System.out.println("""hardQuickEncoded  : List[patmat.HuffmanTest.Bit] = """ + $show(hardQuickEncoded ));$skip(72); 
	val easyQuickDecoded = decode(easyCodeTree, easyQuickEncoded).mkString;System.out.println("""easyQuickDecoded  : String = """ + $show(easyQuickDecoded ));$skip(69); 
	val medQuickDecoded = decode(medCodeTree, medQuickEncoded).mkString;System.out.println("""medQuickDecoded  : String = """ + $show(medQuickDecoded ));$skip(72); 
	val hardQuickDecoded = decode(hardCodeTree, hardQuickEncoded).mkString;System.out.println("""hardQuickDecoded  : String = """ + $show(hardQuickDecoded ))}
}
