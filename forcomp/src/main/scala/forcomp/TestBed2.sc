package forcomp

object TestBed2 {
	// The last thing I need to do is combine the combos list
	// with the list of all the combos with the new char
	// ie. List(combos w/ char) ::: List(combos before adding char)

  def add(c: Char, combos: List[String]): List[String] = combos match {
    case Nil => List(c.toString)
    case x :: xs => c + x :: x :: add(c, xs)
  }                                               //> add: (c: Char, combos: List[String])List[String]

  def combine(r: List[Char]): List[String] = r match {
    case Nil => List()
    case x :: xs => add(x, combine(xs))
  }                                               //> combine: (r: List[Char])List[String]
  
  combine(List('a','b','c','d'))                  //> res0: List[String] = List(abcd, bcd, acd, cd, abd, bd, ad, d, abc, bc, ac, c
                                                  //| , ab, b, a)
  combine(List('a','a','b','c'))                  //> res1: List[String] = List(aabc, abc, abc, bc, aac, ac, ac, c, aab, ab, ab, b
                                                  //| , aa, a, a)
}