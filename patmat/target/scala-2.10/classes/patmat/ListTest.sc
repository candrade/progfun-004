package patmat

object ListTest {
  // type parameters (generics/templates) are done like so:
  trait List[T] {
  	def isEmpty: Boolean
  	def head: T
  	def tail: List[T]
  }
  
  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  	def isEmpty = false
  }
  
  class Nil[T] extends List[T] {
  	def isEmpty = true
  	def head = throw new NoSuchElementException("Nil.head")
  	def tail = throw new NoSuchElementException("Nil.tail")
  }
  
  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
                                                  //> singleton: [T](elem: T)patmat.ListTest.Cons[T]
  
  singleton(1)                                    //> res0: patmat.ListTest.Cons[Int] = patmat.ListTest$Cons@8262adc
  singleton(true)                                 //> res1: patmat.ListTest.Cons[Boolean] = patmat.ListTest$Cons@1c7d5b2
	// "Type erasure": where types are only considered at compilation but not run-time
	
	def nth[T](n: Int, xs: List[T]): T =
		if (n == 0) xs.head
		else nth(n-1, xs.tail)            //> nth: [T](n: Int, xs: patmat.ListTest.List[T])T
		
	val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
                                                  //> list  : patmat.ListTest.Cons[Int] = patmat.ListTest$Cons@669e9a07
	nth(2, list)                              //> res2: Int = 3
	//nth(4, list)
	
	// Functions are objects with apply methods
	
	object List {
		// List(1, 2) = List.apply(1, 2)
		def apply[T](x1: T, x2: T) : List[T] = new Cons(x1, new Cons(x2, new Nil))
		def apply[T]() = new Nil
	}
	
	// Type Bounds
	// "<:" means it is an upper bound = subtypes
	// ">:" means it is an lower bound = supertypes
	// ">: myType <:" mixed bounds = inbetween types
	
	// Variance
	// "covariant" means the subtyping relationship varies with the type parameter
	//		ie. => List[NonEmpty] <: List[IntSet]
	// In Scala arrays are not covariant, so an array of a subtype cannot be set to an array of a supertype
	
	
	// Peano Numbers ("Nat" means natural numbers)
	abstract class Nat {
		def isZero: Boolean
		def predecessor: Nat
		def successor = new Succ(this)
		def +(that: Nat): Nat
		def -(that: Nat): Nat
	}
	
	object Zero extends Nat {
		def isZero = true
		def predecessor = throw new Error("0.predecessor")
		def +(that: Nat) = that
		def -(that: Nat) =
			if (that.isZero) this
			else throw new Error("Negative number")
	}
	
	class Succ(n: Nat) extends Nat {
		def isZero = false
		def predecessor = n
		def +(that: Nat) = new Succ(n + that)
		def -(that: Nat) =
			if (that.isZero) this
			else n - that.predecessor
	}
	
	// Pattern Matching
	trait Expr
	case class Number(n: Int) extends Expr
	case class Sum(e1: Expr, e2: Expr) extends Expr
	
	def show (e: Expr): String = e match {
		case Number(x) => x.toString
		case Sum(l, r) => show(l) + " + " + show(r)
	}                                         //> show: (e: patmat.ListTest.Expr)String
	
	show(Sum(Number(1), Number(44)))          //> res3: String = 1 + 44
	
	// Lists
	//
	// All lists are constructed from:
	//	- the empty list Nil
	//	- the construction operation "::" which is the syntax for the "cons" function
	//		so "x::xs" give us a new list of x as head and xs as the tail.
	val fruit = "apples" :: ("oranges" :: ("pears" :: Nil))
                                                  //> fruit  : List[String] = List(apples, oranges, pears)
	// or (This is because operators associate to the right)
	val newFruit = "longan" :: "melons" :: "mangos" :: Nil
                                                  //> newFruit  : List[String] = List(longan, melons, mangos)
	
	val fruitBowl = fruit ::: newFruit        //> fruitBowl  : List[String] = List(apples, oranges, pears, longan, melons, ma
                                                  //| ngos)
  val fruitCount: (String, Int) = ("mango", 2)    //> fruitCount  : (String, Int) = (mango,2)
  val fruitType = fruitCount._1                   //> fruitType  : String = mango
  val fruitNum = fruitCount._2                    //> fruitNum  : Int = 2
                  
	// We can also pattern match on cons
	/*
	def insert(x: Int, xs: List[Int]): List[Int] = xs match {
		case List() => List(x)
		case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
	}
	
	def isort(xs: List[Int]): List[Int] = xs match {
		case List() => List()
		case y :: ys => insert(y, isort(ys))
	}
	*/
}