package patmat

object incrtest {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(232); 
  def incr(c: Char, acc: List[(Char, Int)]): List[(Char, Int)] = acc match {
    case Nil => List((c, 1))
    case y :: ys =>
      if (c == y._1) (c, y._2 + 1) :: ys
      else y :: incr(c, ys)
  };System.out.println("""incr: (c: Char, acc: List[(Char, Int)])List[(Char, Int)]""");$skip(174); 

  def count(x: Char, xs: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = xs match {
    case Nil => incr(x, acc)
    case y :: ys => count(y, ys, incr(x, acc))
  };System.out.println("""count: (x: Char, xs: List[Char], acc: List[(Char, Int)])List[(Char, Int)]""");$skip(135); 

  def times(chars: List[Char]): List[(Char, Int)] = chars match {
    case Nil => List()
    case x :: xs => count(x, xs, List())
  };System.out.println("""times: (chars: List[Char])List[(Char, Int)]""");$skip(31); val res$0 = 

	times(List('a','a','a','b'));System.out.println("""res0: List[(Char, Int)] = """ + $show(res$0))}
}
