package patmat

import math.Ordering

object ListTest2 {
	def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
		val n = xs.length / 2
		if (n == 0) xs
		else {
			def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
				case (Nil, ys) => ys
				case (xs, Nil) => xs
				case (x :: xs1, y :: ys1) =>
					if (ord.lt(x,y)) x :: merge(xs1, ys)
					else y :: merge(xs, ys1)
			}
			
			val (fst, snd) = xs splitAt n
			merge(msort(fst), msort(snd))
		}
	}                                         //> msort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]
	
	val fruits = List("bananas","mangos","pineapples","apples")
                                                  //> fruits  : List[String] = List(bananas, mangos, pineapples, apples)
	msort(fruits)                             //> res0: List[String] = List(apples, bananas, mangos, pineapples)
	val nums = List(2, -4, 5, 7, 1)           //> nums  : List[Int] = List(2, -4, 5, 7, 1)
	msort(nums)                               //> res1: List[Int] = List(-4, 1, 2, 5, 7)
	
	// Takes until the condition is met, also returns what's after
	nums span (x => x > 0)                    //> res2: (List[Int], List[Int]) = (List(2),List(-4, 5, 7, 1))
	
	def pack[T](xs: List[T]): List[List[T]] = xs match {
		case Nil => Nil
		case x :: xs1 =>
			val (first, rest) = xs span (y => y == x)
			first :: pack(rest)
	}                                         //> pack: [T](xs: List[T])List[List[T]]
	
	def encode[T](xs: List[T]): List[(T, Int)] =
		pack(xs) map (ys => (ys.head, ys.length))
                                                  //> encode: [T](xs: List[T])List[(T, Int)]
}