package recfun

import java.lang

object recfun {
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
  }                                               //> countChange: (money: Int, coins: List[Int])Int
  
	// 3 ways to give change for 4 if you have coins with denomiation 1 and 2: 1+1+1+1, 1+1+2, 2+2.
  countChange(4, List(1,2))                       //> recurse
                                                  //| recurse
                                                  //| recurse
                                                  //| recurse
                                                  //| valid
                                                  //| recurse
                                                  //| valid
                                                  //| valid
                                                  //| recurse
                                                  //| valid
                                                  //| valid
                                                  //| recurse
                                                  //| recurse
                                                  //| valid
                                                  //| valid
                                                  //| valid
                                                  //| recurse
                                                  //| recurse
                                                  //| valid
                                                  //| valid
                                                  //| valid
                                                  //| res0: Int = 3
  //countChange(301,List(5,10,20,50,100,200,500)) == 0
  //countChange(4, List(1,2)) == 3
}