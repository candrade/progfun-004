package patmat

import common._

object HuffmanTest {
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree): Int = tree match {
    case Fork(l, r, cs, w) => w
    case Leaf(c, w) => w
  }                                               //> weight: (tree: patmat.HuffmanTest.CodeTree)Int

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(l, r, cs, w) => cs
    case Leaf(c, w) => List(c)
  }                                               //> chars: (tree: patmat.HuffmanTest.CodeTree)List[Char]

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
                                                  //> makeCodeTree: (left: patmat.HuffmanTest.CodeTree, right: patmat.HuffmanTest.
                                                  //| CodeTree)patmat.HuffmanTest.Fork
  
  def insert[T](comparator: (T, T) => Boolean)(x: T, xs: List[T]): List[T] = xs match {
    case List() => List(x)
    case y :: ys =>
      if (comparator(x, y)) x :: xs
      else y :: insert(comparator)(x, ys)
  }                                               //> insert: [T](comparator: (T, T) => Boolean)(x: T, xs: List[T])List[T]
	
  def count(c: Char, acc: List[(Char, Int)]): List[(Char, Int)] = acc match {
    case Nil => List((c, 1))
    case y :: ys =>
      if (c == y._1) (c, y._2 + 1) :: ys
      else y :: count(c, ys)
  }                                               //> count: (c: Char, acc: List[(Char, Int)])List[(Char, Int)]
  
  def times(chars: List[Char]): List[(Char, Int)] = chars match {
    case Nil => List()
    case x :: xs => count(x, times(xs))
  }                                               //> times: (chars: List[Char])List[(Char, Int)]

	times(List('a','a','z','z','z','z','a','b','c','b'))
                                                  //> res0: List[(Char, Int)] = List((b,2), (c,1), (a,3), (z,4))

	def leafListInsert = insert[Leaf]((l1, l2) => l1.weight <= l2.weight) _
                                                  //> leafListInsert: => (patmat.HuffmanTest.Leaf, List[patmat.HuffmanTest.Leaf])
                                                  //|  => List[patmat.HuffmanTest.Leaf]
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs match {
  	case Nil => List()
  	case y :: ys =>
  		leafListInsert(Leaf(y._1, y._2), makeOrderedLeafList(ys))
  }                                               //> makeOrderedLeafList: (freqs: List[(Char, Int)])List[patmat.HuffmanTest.Leaf
                                                  //| ]
                                              
	def singleton(trees: List[CodeTree]): Boolean = trees match {
    case y :: Nil => true
    case _ => false
  }                                               //> singleton: (trees: List[patmat.HuffmanTest.CodeTree])Boolean

	def codeTreeInsert = insert[CodeTree]((t1, t2) => weight(t1) <= weight(t2)) _
                                                  //> codeTreeInsert: => (patmat.HuffmanTest.CodeTree, List[patmat.HuffmanTest.Co
                                                  //| deTree]) => List[patmat.HuffmanTest.CodeTree]
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case Nil => Nil
    case x :: y :: rest => codeTreeInsert(makeCodeTree(x, y), rest)
    case y :: Nil => List(y)
  }                                               //> combine: (trees: List[patmat.HuffmanTest.CodeTree])List[patmat.HuffmanTest.
                                                  //| CodeTree]

	val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
                                                  //> leaflist  : List[patmat.HuffmanTest.Leaf] = List(Leaf(e,1), Leaf(t,2), Leaf
                                                  //| (x,4))
	val combined = combine(leaflist)          //> combined  : List[patmat.HuffmanTest.CodeTree] = List(Fork(Leaf(e,1),Leaf(t,
                                                  //| 2),List(e, t),3), Leaf(x,4))
	//val finalCombined = combine(combined)
    
  def until(t: List[CodeTree] => Boolean, c: List[CodeTree] => List[CodeTree])(ts: List[CodeTree]): List[CodeTree] =
    if (t(ts)) ts
    else until(t, c)(c(ts))                       //> until: (t: List[patmat.HuffmanTest.CodeTree] => Boolean, c: List[patmat.Huf
                                                  //| fmanTest.CodeTree] => List[patmat.HuffmanTest.CodeTree])(ts: List[patmat.Hu
                                                  //| ffmanTest.CodeTree])List[patmat.HuffmanTest.CodeTree]

  def createCodeTree(chars: List[Char]): CodeTree = chars match {
    case List(x, _*) =>
      until(singleton, combine)(makeOrderedLeafList(times(chars))).head
    case Nil => Leaf('0', 0)
  }                                               //> createCodeTree: (chars: List[Char])patmat.HuffmanTest.CodeTree
  
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
  }                                               //> decode: (tree: patmat.HuffmanTest.CodeTree, bits: List[patmat.HuffmanTest.B
                                                  //| it])List[Char]
  
  def contains(char: Char, chars: List[Char]): Boolean =
  	if (chars.isEmpty) false
  	else if (chars.head == char) true
  	else contains(char, chars.tail)           //> contains: (char: Char, chars: List[Char])Boolean
  	
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
  }                                               //> encode: (tree: patmat.HuffmanTest.CodeTree)(text: List[Char])List[patmat.Hu
                                                  //| ffmanTest.Bit]
  
  val easyTest = "ababaz".toList                  //> easyTest  : List[Char] = List(a, b, a, b, a, z)
  val easyResult = times(easyTest)                //> easyResult  : List[(Char, Int)] = List((z,1), (a,3), (b,2))
  val orderedEasyResult = makeOrderedLeafList(easyResult)
                                                  //> orderedEasyResult  : List[patmat.HuffmanTest.Leaf] = List(Leaf(z,1), Leaf(b
                                                  //| ,2), Leaf(a,3))
  val easyCodeTree = createCodeTree(easyTest)     //> easyCodeTree  : patmat.HuffmanTest.CodeTree = Fork(Fork(Leaf(z,1),Leaf(b,2)
                                                  //| ,List(z, b),3),Leaf(a,3),List(z, b, a),6)
  
  val medTest = "hello world".toList              //> medTest  : List[Char] = List(h, e, l, l, o,  , w, o, r, l, d)
  val medResult = times(medTest)                  //> medResult  : List[(Char, Int)] = List((d,1), (l,3), (r,1), (o,2), (w,1), ( 
                                                  //| ,1), (e,1), (h,1))
  val orderedMedResult = makeOrderedLeafList(medResult)
                                                  //> orderedMedResult  : List[patmat.HuffmanTest.Leaf] = List(Leaf(d,1), Leaf(r,
                                                  //| 1), Leaf(w,1), Leaf( ,1), Leaf(e,1), Leaf(h,1), Leaf(o,2), Leaf(l,3))
  val medCodeTree = createCodeTree(medTest)       //> medCodeTree  : patmat.HuffmanTest.CodeTree = Fork(Fork(Fork(Leaf(e,1),Leaf(
                                                  //| h,1),List(e, h),2),Fork(Leaf(w,1),Leaf( ,1),List(w,  ),2),List(e, h, w,  ),
                                                  //| 4),Fork(Leaf(l,3),Fork(Fork(Leaf(d,1),Leaf(r,1),List(d, r),2),Leaf(o,2),Lis
                                                  //| t(d, r, o),4),List(l, d, r, o),7),List(e, h, w,  , l, d, r, o),11)

	val exceptionTest = createCodeTree("".toList)
                                                  //> exceptionTest  : patmat.HuffmanTest.CodeTree = Leaf(0,0)
	val easyEncodedTest = encode(easyCodeTree)(easyTest)
                                                  //> easyEncodedTest  : List[patmat.HuffmanTest.Bit] = List(1, 0, 1, 1, 0, 1, 1,
                                                  //|  0, 0)
	val easyDecodedTest = decode(easyCodeTree, easyEncodedTest).mkString
                                                  //> easyDecodedTest  : String = ababaz
	
	val medEncodedTest = encode(medCodeTree)(medTest)
                                                  //> medEncodedTest  : List[patmat.HuffmanTest.Bit] = List(0, 0, 1, 0, 0, 0, 1, 
                                                  //| 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0)
	val medDecodedTest = decode(medCodeTree, medEncodedTest).mkString
                                                  //> medDecodedTest  : String = hello world

  val hardTest = "Because the Java application is run from within a Map-Reduce job".toList
                                                  //> hardTest  : List[Char] = List(B, e, c, a, u, s, e,  , t, h, e,  , J, a, v, 
                                                  //| a,  , a, p, p, l, i, c, a, t, i, o, n,  , i, s,  , r, u, n,  , f, r, o, m, 
                                                  //|  , w, i, t, h, i, n,  , a,  , M, a, p, -, R, e, d, u, c, e,  , j, o, b)
  val hardCodeTree = createCodeTree(hardTest)     //> hardCodeTree  : patmat.HuffmanTest.CodeTree = Fork(Fork(Fork(Fork(Leaf(c,3)
                                                  //| ,Leaf(u,3),List(c, u),6),Fork(Fork(Leaf(B,1),Fork(Leaf(v,1),Leaf(J,1),List(
                                                  //| v, J),2),List(B, v, J),3),Leaf(o,3),List(B, v, J, o),6),List(c, u, B, v, J,
                                                  //|  o),12),Fork(Fork(Leaf(t,3),Fork(Leaf(r,2),Leaf(s,2),List(r, s),4),List(t, 
                                                  //| r, s),7),Leaf(a,7),List(t, r, s, a),14),List(c, u, B, v, J, o, t, r, s, a),
                                                  //| 26),Fork(Fork(Fork(Fork(Fork(Leaf(b,1),Leaf(j,1),List(b, j),2),Leaf(h,2),Li
                                                  //| st(b, j, h),4),Fork(Fork(Leaf(-,1),Leaf(M,1),List(-, M),2),Fork(Leaf(d,1),L
                                                  //| eaf(R,1),List(d, R),2),List(-, M, d, R),4),List(b, j, h, -, M, d, R),8),For
                                                  //| k(Fork(Fork(Leaf(f,1),Leaf(l,1),List(f, l),2),Fork(Leaf(w,1),Leaf(m,1),List
                                                  //| (w, m),2),List(f, l, w, m),4),Leaf(e,5),List(f, l, w, m, e),9),List(b, j, h
                                                  //| , -, M, d, R, f, l, w, m, e),17),Fork(Leaf( ,10),Fork(Leaf(i,5),Fork(Leaf(p
                                                  //| ,3),Leaf(n,3),List(p, n),6),List(i, p, n),11),List( , i, p, n),21),List(b, 
                                                  //| j, h, -, M, d, R, f, l,
                                                  //| Output exceeds cutoff limit.
  val hardEncodedTest = encode(hardCodeTree)(hardTest)
                                                  //> hardEncodedTest  : List[patmat.HuffmanTest.Bit] = List(0, 0, 1, 0, 0, 1, 0,
                                                  //|  1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0,
                                                  //|  0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1,
                                                  //|  0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0,
                                                  //|  1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0,
                                                  //|  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0,
                                                  //|  1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0,
                                                  //|  1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0,
                                                  //|  0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1,
                                                  //|  1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0,
                                                  //|  1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1,
                                                  //|  1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0)
  val hardDecodedTest = decode(hardCodeTree, hardEncodedTest).mkString
                                                  //> hardDecodedTest  : String = Because the Java application is run from within
                                                  //|  a Map-Reduce job
            
	type Bit = Int
  
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)
                                                  //> frenchCode  : patmat.HuffmanTest.CodeTree = Fork(Fork(Fork(Leaf(s,121895),F
                                                  //| ork(Leaf(d,56269),Fork(Fork(Fork(Leaf(x,5928),Leaf(j,8351),List(x, j),14279
                                                  //| ),Leaf(f,16351),List(x, j, f),30630),Fork(Fork(Fork(Fork(Leaf(z,2093),Fork(
                                                  //| Leaf(k,745),Leaf(w,1747),List(k, w),2492),List(z, k, w),4585),Leaf(y,4725),
                                                  //| List(z, k, w, y),9310),Leaf(h,11298),List(z, k, w, y, h),20608),Leaf(q,2088
                                                  //| 9),List(z, k, w, y, h, q),41497),List(x, j, f, z, k, w, y, h, q),72127),Lis
                                                  //| t(d, x, j, f, z, k, w, y, h, q),128396),List(s, d, x, j, f, z, k, w, y, h, 
                                                  //| q),250291),Fork(Fork(Leaf(o,82762),Leaf(l,83668),List(o, l),166430),Fork(Fo
                                                  //| rk(Leaf(m,45521),Leaf(p,46335),List(m, p),91856),Leaf(u,96785),List(m, p, u
                                                  //| ),188641),List(o, l, m, p, u),355071),List(s, d, x, j, f, z, k, w, y, h, q,
                                                  //|  o, l, m, p, u),605362),Fork(Fork(Fork(Leaf(r,100500),Fork(Leaf(c,50003),Fo
                                                  //| rk(Leaf(v,24975),Fork(Leaf(g,13288),Leaf(b,13822),List(g, b),27110),List(v,
                                                  //|  g, b),52085),List(c, v
                                                  //| Output exceeds cutoff limit.
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
                                                  //> secret  : List[patmat.HuffmanTest.Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1,
                                                  //|  1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1,
                                                  //|  0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
                                                  //|  1, 0, 1)

  val decodedSecret = decode(frenchCode, secret).mkString
                                                  //> decodedSecret  : String = huffmanestcool
  
  
  
  type CodeTable = List[(Char, List[Bit])]
  
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a match {
    case Nil => b
    case x :: xs => x :: mergeCodeTables(xs, b)
  }                                               //> mergeCodeTables: (a: patmat.HuffmanTest.CodeTable, b: patmat.HuffmanTest.Co
                                                  //| deTable)patmat.HuffmanTest.CodeTable
  
  def convert(tree: CodeTree): CodeTable = {
    def build(subTree: CodeTree, acc: List[Bit]): CodeTable = subTree match {
      case Fork(l, r, cs, w) =>
        mergeCodeTables(build(l, acc ::: List(0)), build(r, acc ::: List(1)))
      case Leaf(c, w) => List((c, acc))
    }
    
    build(tree, List())
  }                                               //> convert: (tree: patmat.HuffmanTest.CodeTree)patmat.HuffmanTest.CodeTable
  
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case Nil => List()
    case x :: xs =>
      if (x._1 == char) x._2
      else codeBits(xs)(char)
  }                                               //> codeBits: (table: patmat.HuffmanTest.CodeTable)(char: Char)List[patmat.Huff
                                                  //| manTest.Bit]
  
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def generate(t: CodeTable, xs: List[Char]): List[Bit] = xs match {
      case Nil => Nil
      case x :: xs1 => codeBits(t)(x) ::: generate(t, xs1)
    }
    
    generate(convert(tree), text)
  }                                               //> quickEncode: (tree: patmat.HuffmanTest.CodeTree)(text: List[Char])List[patm
                                                  //| at.HuffmanTest.Bit]
  
  val tableA = List(('a', List(0,0,1)), ('b', List(0,1,0)), ('c', List(0,1,1)))
                                                  //> tableA  : List[(Char, List[Int])] = List((a,List(0, 0, 1)), (b,List(0, 1, 0
                                                  //| )), (c,List(0, 1, 1)))
  val tableB = List(('d', List(1,0,0)), ('e', List(1,0,1)), ('f', List(1,1,1)))
                                                  //> tableB  : List[(Char, List[Int])] = List((d,List(1, 0, 0)), (e,List(1, 0, 1
                                                  //| )), (f,List(1, 1, 1)))
  
  mergeCodeTables(tableA, tableB)                 //> res1: patmat.HuffmanTest.CodeTable = List((a,List(0, 0, 1)), (b,List(0, 1, 
                                                  //| 0)), (c,List(0, 1, 1)), (d,List(1, 0, 0)), (e,List(1, 0, 1)), (f,List(1, 1,
                                                  //|  1)))
  
  val easyCodeTable = convert(easyCodeTree)       //> easyCodeTable  : patmat.HuffmanTest.CodeTable = List((z,List(0, 0)), (b,Lis
                                                  //| t(0, 1)), (a,List(1)))
  
  val medCodeTable = convert(medCodeTree)         //> medCodeTable  : patmat.HuffmanTest.CodeTable = List((e,List(0, 0, 0)), (h,L
                                                  //| ist(0, 0, 1)), (w,List(0, 1, 0)), ( ,List(0, 1, 1)), (l,List(1, 0)), (d,Lis
                                                  //| t(1, 1, 0, 0)), (r,List(1, 1, 0, 1)), (o,List(1, 1, 1)))
                                                   
  val bitsForA = codeBits(easyCodeTable)('a')     //> bitsForA  : List[patmat.HuffmanTest.Bit] = List(1)
  val bitsForH = codeBits(medCodeTable)('h')      //> bitsForH  : List[patmat.HuffmanTest.Bit] = List(0, 0, 1)
  val easyQuickEncoded = quickEncode(easyCodeTree)(easyTest)
                                                  //> easyQuickEncoded  : List[patmat.HuffmanTest.Bit] = List(1, 0, 1, 1, 0, 1, 1
                                                  //| , 0, 0)
  val medQuickEncoded = quickEncode(medCodeTree)(medTest)
                                                  //> medQuickEncoded  : List[patmat.HuffmanTest.Bit] = List(0, 0, 1, 0, 0, 0, 1,
                                                  //|  0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0)
                                                  //| 
  val hardQuickEncoded = quickEncode(hardCodeTree)(hardTest)
                                                  //> hardQuickEncoded  : List[patmat.HuffmanTest.Bit] = List(0, 0, 1, 0, 0, 1, 0
                                                  //| , 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0
                                                  //| , 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1
                                                  //| , 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0
                                                  //| , 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0
                                                  //| , 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0
                                                  //| , 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0
                                                  //| , 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0
                                                  //| , 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1
                                                  //| , 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0
                                                  //| , 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1
                                                  //| , 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0)
	val easyQuickDecoded = decode(easyCodeTree, easyQuickEncoded).mkString
                                                  //> easyQuickDecoded  : String = ababaz
	val medQuickDecoded = decode(medCodeTree, medQuickEncoded).mkString
                                                  //> medQuickDecoded  : String = hello world
	val hardQuickDecoded = decode(hardCodeTree, hardQuickEncoded).mkString
                                                  //> hardQuickDecoded  : String = Because the Java application is run from withi
                                                  //| n a Map-Reduce job
}