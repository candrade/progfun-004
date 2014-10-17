package recfun

import java.lang

object recfun {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(621); 
	// Hint: Think of the degenerate cases.
	// How many ways can you give change for 0 CHF?
	// How many ways can you give change for >0 CHF, if you have no coins?

	// Returns: the amount of ways the money can be split using the denominations
  def countChange(money: Int, coins: List[Int]): Int = {
  	if (money == 0) println("valid")
  	else if (coins.isEmpty || money < 0) println("valid")
  	else println("recurse")
		
		if (money == 0) 1
		else if (coins.isEmpty || money < 0) 0
		else
			countChange(money - coins.head, coins) +
			countChange(money, coins.tail)
  };System.out.println("""countChange: (money: Int, coins: List[Int])Int""");$skip(128); val res$0 = 
  
	// 3 ways to give change for 4 if you have coins with denomiation 1 and 2: 1+1+1+1, 1+1+2, 2+2.
  countChange(4, List(1,2));System.out.println("""res0: Int = """ + $show(res$0))}
  //countChange(301,List(5,10,20,50,100,200,500)) == 0
  //countChange(4, List(1,2)) == 3
}
