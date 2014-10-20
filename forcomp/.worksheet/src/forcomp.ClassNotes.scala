package forcomp

object ClassNotes {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(63); 
  val r: Range = 1 until 5;System.out.println("""r  : Range = """ + $show(r ));$skip(24); 
  val s: Range = 1 to 5;System.out.println("""s  : Range = """ + $show(s ));$skip(33); 
	val evens: Range = 0 to 10 by 2;System.out.println("""evens  : Range = """ + $show(evens ));$skip(26); 
  val str = "hello world";System.out.println("""str  : String = """ + $show(str ));$skip(42); 
  val pairs = List(1, 2, 3) zip str unzip;System.out.println("""pairs  : (List[Int], List[Char]) = """ + $show(pairs ));$skip(14); 

  val M = 10;System.out.println("""M  : Int = """ + $show(M ));$skip(13); 
  val N = 10;System.out.println("""N  : Int = """ + $show(N ));$skip(146); val res$0 = 

  // List all combos of numbers x and y where x is drawn from 1..M and y is drawn from 1..N
  (1 to M) flatMap (x => (1 to N) map (y => (x, y)));System.out.println("""res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = """ + $show(res$0));$skip(74); 

  
  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0);System.out.println("""isPrime: (n: Int)Boolean""");$skip(12); 
  val n = 7;System.out.println("""n  : Int = """ + $show(n ));$skip(110); val res$1 = 
  (1 until n) flatMap (i =>
  	(1 until i) map (j => (i, j))) filter (pair =>
  		isPrime(pair._1 + pair._2));System.out.println("""res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = """ + $show(res$1));$skip(128); val res$2 = 
                                                  
	for {
		i <- 1 until n
		j <- 1 until i
		if isPrime(i + j)
	} yield (i, j);System.out.println("""res2: scala.collection.immutable.IndexedSeq[(Int, Int)] = """ + $show(res$2));$skip(112); 

	def scalarProduct(xs: List[Double], ys: List[Double]): Double =
		(for ((x, y) <- xs zip ys) yield x * y).sum;System.out.println("""scalarProduct: (xs: List[Double], ys: List[Double])Double""");$skip(46); 

	val romanNumerals = Map("I" -> 1, "V" -> 5);System.out.println("""romanNumerals  : scala.collection.immutable.Map[String,Int] = """ + $show(romanNumerals ));$skip(23); val res$3 = 
	romanNumerals get "X";System.out.println("""res3: Option[Int] = """ + $show(res$3));$skip(23); val res$4 = 
	romanNumerals get "V";System.out.println("""res4: Option[Int] = """ + $show(res$4));$skip(139); 
	
	def showNumeralValue(numeral: String) = romanNumerals.get(numeral) match {
		case Some(value) => value
		case None => "missing data"
	};System.out.println("""showNumeralValue: (numeral: String)Any""");$skip(172); 
	
	// Maps are partial functions; because (key) => value. Make into full function with "withDefaultValue"
	def safeRomanNumerals = romanNumerals withDefaultValue "missing";System.out.println("""safeRomanNumerals: => scala.collection.immutable.Map[String,Any]""");$skip(24); val res$5 = 
	safeRomanNumerals("X");System.out.println("""res5: Any = """ + $show(res$5))}
}
