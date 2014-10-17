package patmat

object incrtest {
  def incr(c: Char, acc: List[(Char, Int)]): List[(Char, Int)] = acc match {
    case Nil => List((c, 1))
    case y :: ys =>
      if (c == y._1) (c, y._2 + 1) :: ys
      else y :: incr(c, ys)
  }                                               //> incr: (c: Char, acc: List[(Char, Int)])List[(Char, Int)]

  def count(x: Char, xs: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = xs match {
    case Nil => incr(x, acc)
    case y :: ys => count(y, ys, incr(x, acc))
  }                                               //> count: (x: Char, xs: List[Char], acc: List[(Char, Int)])List[(Char, Int)]

  def times(chars: List[Char]): List[(Char, Int)] = chars match {
    case Nil => List()
    case x :: xs => count(x, xs, List())
  }                                               //> times: (chars: List[Char])List[(Char, Int)]

	times(List('a','a','a','b'))              //> res0: List[(Char, Int)] = List((a,3), (b,1))
}