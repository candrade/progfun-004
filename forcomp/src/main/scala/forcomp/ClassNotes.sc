package forcomp

object ClassNotes {
  val r: Range = 1 until 5                        //> r  : Range = Range(1, 2, 3, 4)
  val s: Range = 1 to 5                           //> s  : Range = Range(1, 2, 3, 4, 5)
	val evens: Range = 0 to 10 by 2           //> evens  : Range = Range(0, 2, 4, 6, 8, 10)
  val str = "hello world"                         //> str  : String = hello world
  val pairs = List(1, 2, 3) zip str unzip         //> pairs  : (List[Int], List[Char]) = (List(1, 2, 3),List(h, e, l))

  val M = 10                                      //> M  : Int = 10
  val N = 10                                      //> N  : Int = 10

  // List all combos of numbers x and y where x is drawn from 1..M and y is drawn from 1..N
  (1 to M) flatMap (x => (1 to N) map (y => (x, y)))
                                                  //> res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (1,2
                                                  //| ), (1,3), (1,4), (1,5), (1,6), (1,7), (1,8), (1,9), (1,10), (2,1), (2,2), (2
                                                  //| ,3), (2,4), (2,5), (2,6), (2,7), (2,8), (2,9), (2,10), (3,1), (3,2), (3,3), 
                                                  //| (3,4), (3,5), (3,6), (3,7), (3,8), (3,9), (3,10), (4,1), (4,2), (4,3), (4,4)
                                                  //| , (4,5), (4,6), (4,7), (4,8), (4,9), (4,10), (5,1), (5,2), (5,3), (5,4), (5,
                                                  //| 5), (5,6), (5,7), (5,8), (5,9), (5,10), (6,1), (6,2), (6,3), (6,4), (6,5), (
                                                  //| 6,6), (6,7), (6,8), (6,9), (6,10), (7,1), (7,2), (7,3), (7,4), (7,5), (7,6),
                                                  //|  (7,7), (7,8), (7,9), (7,10), (8,1), (8,2), (8,3), (8,4), (8,5), (8,6), (8,7
                                                  //| ), (8,8), (8,9), (8,10), (9,1), (9,2), (9,3), (9,4), (9,5), (9,6), (9,7), (9
                                                  //| ,8), (9,9), (9,10), (10,1), (10,2), (10,3), (10,4), (10,5), (10,6), (10,7), 
                                                  //| (10,8), (10,9), (10,10))

  
  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
                                                  //> isPrime: (n: Int)Boolean
  val n = 7                                       //> n  : Int = 7
  (1 until n) flatMap (i =>
  	(1 until i) map (j => (i, j))) filter (pair =>
  		isPrime(pair._1 + pair._2))       //> res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5))
                                                  
	for {
		i <- 1 until n
		j <- 1 until i
		if isPrime(i + j)
	} yield (i, j)                            //> res2: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5))

	def scalarProduct(xs: List[Double], ys: List[Double]): Double =
		(for ((x, y) <- xs zip ys) yield x * y).sum
                                                  //> scalarProduct: (xs: List[Double], ys: List[Double])Double

	val romanNumerals = Map("I" -> 1, "V" -> 5)
                                                  //> romanNumerals  : scala.collection.immutable.Map[String,Int] = Map(I -> 1, V 
                                                  //| -> 5)
	romanNumerals get "X"                     //> res3: Option[Int] = None
	romanNumerals get "V"                     //> res4: Option[Int] = Some(5)
	
	def showNumeralValue(numeral: String) = romanNumerals.get(numeral) match {
		case Some(value) => value
		case None => "missing data"
	}                                         //> showNumeralValue: (numeral: String)Any
	
	// Maps are partial functions; because (key) => value. Make into full function with "withDefaultValue"
	def safeRomanNumerals = romanNumerals withDefaultValue "missing"
                                                  //> safeRomanNumerals: => scala.collection.immutable.Map[String,Any]
	safeRomanNumerals("X")                    //> res5: Any = missing
}