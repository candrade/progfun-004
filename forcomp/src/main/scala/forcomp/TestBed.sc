package forcomp

object TestBed {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  
  val dictionary: List[Word] = loadDictionary     //> dictionary  : List[forcomp.TestBed.Word] = List(Aarhus, Aaron, Ababa, aback,
                                                  //|  abaft, abandon, abandoned, abandoning, abandonment, abandons, abase, abased
                                                  //| , abasement, abasements, abases, abash, abashed, abashes, abashing, abasing,
                                                  //|  abate, abated, abatement, abatements, abater, abates, abating, Abba, abbe, 
                                                  //| abbey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbreviates, 
                                                  //| abbreviating, abbreviation, abbreviations, Abby, abdomen, abdomens, abdomina
                                                  //| l, abduct, abducted, abduction, abductions, abductor, abductors, abducts, Ab
                                                  //| e, abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberration, 
                                                  //| aberrations, abet, abets, abetted, abetter, abetting, abeyance, abhor, abhor
                                                  //| red, abhorrent, abhorrer, abhorring, abhors, abide, abided, abides, abiding,
                                                  //|  Abidjan, Abigail, Abilene, abilities, ability, abject, abjection, abjection
                                                  //| s, abjectly, abjectness, abjure, abjured, abjures, abjuring, ablate, ablated
                                                  //| , ablates, ablating, abl
                                                  //| Output exceeds cutoff limit.

  def wordOccurrences(w: Word): Occurrences =
    w.toLowerCase().toList
      .groupBy (s => s)
      .map { case (x, y) => (x, y.length) }
      .toList
      .sorted                                     //> wordOccurrences: (w: forcomp.TestBed.Word)forcomp.TestBed.Occurrences

  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)
                                                  //> sentenceOccurrences: (s: forcomp.TestBed.Sentence)forcomp.TestBed.Occurrence
                                                  //| s
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary
      .map(w => wordOccurrences(w) -> w)
      .groupBy { case (occur, word) => occur }
      .map { case (occur, grouping) => occur -> (grouping map { case (o, word) => word }) }
      .withDefaultValue(Nil)                      //> dictionaryByOccurrences: => Map[forcomp.TestBed.Occurrences,List[forcomp.Tes
                                                  //| tBed.Word]]
  
  dictionaryByOccurrences(List(('a', 1), ('e', 1), ('m', 1), ('n', 1)))
                                                  //> res0: List[forcomp.TestBed.Word] = List(amen, mane, mean, name)
  
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))
                                                  //> wordAnagrams: (word: forcomp.TestBed.Word)List[forcomp.TestBed.Word]
  def testcombinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(List())
    case o :: oo => generator(listMaker(o), testcombinations(oo))
  }                                               //> testcombinations: (occurrences: forcomp.TestBed.Occurrences)List[forcomp.Te
                                                  //| stBed.Occurrences]
  def listMaker(occ: (Char, Int)): List[Occurrences] = {
    val newVector = for (index <- 1 to occ._2) yield (occ._1, index)
    (for (index <- 0 until newVector.length) yield newVector(index) :: Nil).toList ::: List(Nil)
  }                                               //> listMaker: (occ: (Char, Int))List[forcomp.TestBed.Occurrences]

  def generator(occ1: List[Occurrences], occ0: List[Occurrences]): List[Occurrences] =
    for (o1 <- occ1; o0 <- occ0) yield o1 ::: o0  //> generator: (occ1: List[forcomp.TestBed.Occurrences], occ0: List[forcomp.Tes
                                                  //| tBed.Occurrences])List[forcomp.TestBed.Occurrences]


	def pair(acc: String, chars: List[Char]): List[String] =
		for (char <- chars) yield acc + char
                                                  //> pair: (acc: String, chars: List[Char])List[String]
  
  pair("a", List('b','c'))                        //> res1: List[String] = List(ab, ac)

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    val charSeq = for { o <- occurrences; i <- 1 to o._2 } yield o._1

    def add(c: Char, combos: List[String]): List[String] = combos match {
      case Nil => List(c.toString)
      case x :: xs => c + x :: x :: add(c, xs)
    }

    def combine(r: List[Char]): List[String] = r match {
      case Nil => List()
      case x :: xs => add(x, combine(xs))
    }

    (("" :: combine(charSeq)) distinct) map (s => wordOccurrences(s))
  }                                               //> combinations: (occurrences: forcomp.TestBed.Occurrences)List[forcomp.TestBe
                                                  //| d.Occurrences]
  
  testcombinations(List(('a', 2),('b', 1),('c', 1)))
                                                  //> res2: List[forcomp.TestBed.Occurrences] = List(List((a,1), (b,1), (c,1)), L
                                                  //| ist((a,1), (b,1)), List((a,1), (c,1)), List((a,1)), List((a,2), (b,1), (c,1
                                                  //| )), List((a,2), (b,1)), List((a,2), (c,1)), List((a,2)), List((b,1), (c,1))
                                                  //| , List((b,1)), List((c,1)), List())
  combinations(List(('a', 2),('b', 1),('c', 1)))  //> res3: List[forcomp.TestBed.Occurrences] = List(List(), List((a,2), (b,1), (
                                                  //| c,1)), List((a,1), (b,1), (c,1)), List((b,1), (c,1)), List((a,2), (c,1)), L
                                                  //| ist((a,1), (c,1)), List((c,1)), List((a,2), (b,1)), List((a,1), (b,1)), Lis
                                                  //| t((b,1)), List((a,2)), List((a,1)))

	def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    y.foldLeft(x toMap)((map, y1) => {
    	val diff = map(y1._1) - y1._2
    	if (diff > 0) map.updated(y1._1, diff)
			else map - y1._1
    }).toList.sorted
	}                                         //> subtract: (x: forcomp.TestBed.Occurrences, y: forcomp.TestBed.Occurrences)f
                                                  //| orcomp.TestBed.Occurrences
	
	val testSentence = List("Yes","man")      //> testSentence  : List[String] = List(Yes, man)
  val occurances = sentenceOccurrences(testSentence)
                                                  //> occurances  : forcomp.TestBed.Occurrences = List((a,1), (e,1), (m,1), (n,1)
                                                  //| , (s,1), (y,1))
  //val combos = combinations(occurances)
  
  combinations(occurances).length == testcombinations(occurances).length
                                                  //> res4: Boolean = true
  def printInFor[T](s: T): T = {
	  println(s.toString)
	  s
	}                                         //> printInFor: [T](s: T)T
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def compileAnagrams(occurrences: Occurrences): List[Sentence] = {
    	if (occurrences.isEmpty) List(Nil)
    	else
	      for {
	        subset <- combinations(occurrences)
	        word <- dictionaryByOccurrences(subset)
	        rest <- compileAnagrams(subtract(occurrences, subset))
	      } yield word :: rest
    }

    compileAnagrams(sentenceOccurrences(sentence))
  }                                               //> sentenceAnagrams: (sentence: forcomp.TestBed.Sentence)List[forcomp.TestBed.
                                                  //| Sentence]
  combinations(sentenceOccurrences(testSentence)) //> res5: List[forcomp.TestBed.Occurrences] = List(List(), List((a,1), (e,1), (
                                                  //| m,1), (n,1), (s,1), (y,1)), List((e,1), (m,1), (n,1), (s,1), (y,1)), List((
                                                  //| a,1), (m,1), (n,1), (s,1), (y,1)), List((m,1), (n,1), (s,1), (y,1)), List((
                                                  //| a,1), (e,1), (n,1), (s,1), (y,1)), List((e,1), (n,1), (s,1), (y,1)), List((
                                                  //| a,1), (n,1), (s,1), (y,1)), List((n,1), (s,1), (y,1)), List((a,1), (e,1), (
                                                  //| m,1), (s,1), (y,1)), List((e,1), (m,1), (s,1), (y,1)), List((a,1), (m,1), (
                                                  //| s,1), (y,1)), List((m,1), (s,1), (y,1)), List((a,1), (e,1), (s,1), (y,1)), 
                                                  //| List((e,1), (s,1), (y,1)), List((a,1), (s,1), (y,1)), List((s,1), (y,1)), L
                                                  //| ist((a,1), (e,1), (m,1), (n,1), (y,1)), List((e,1), (m,1), (n,1), (y,1)), L
                                                  //| ist((a,1), (m,1), (n,1), (y,1)), List((m,1), (n,1), (y,1)), List((a,1), (e,
                                                  //| 1), (n,1), (y,1)), List((e,1), (n,1), (y,1)), List((a,1), (n,1), (y,1)), Li
                                                  //| st((n,1), (y,1)), List((a,1), (e,1), (m,1), (y,1)), List((e,1), (m,1), (y,1
                                                  //| )), List((a,1), (m,1), 
                                                  //| Output exceeds cutoff limit.
  val result = sentenceAnagrams(testSentence)     //> result  : List[forcomp.TestBed.Sentence] = List(List(yes, man), List(say, m
                                                  //| en), List(my, sane), List(my, Sean), List(my, as, en), List(my, en, as), Li
                                                  //| st(sane, my), List(Sean, my), List(as, my, en), List(as, en, my), List(men,
                                                  //|  say), List(man, yes), List(en, my, as), List(en, as, my))
                                                  
                                                  

  /*

	subtract(List(('a',3),('b',4)), List(('a',1),('b',1)))
	subtract(List(('a',1),('b',1)), List(('a',1),('b',1)))
  val lardd = List(('a', 1), ('l', 1), ('r', 1), ('d', 2))
  val dr = List(('d', 1), ('r', 1))
  val lad = List(('a', 1), ('d', 1), ('l', 1))
  val diff = subtract(lardd, dr)
  diff== lad
	
	findAnagrams(occurances)
		for (subset in findCombos(occurances))
			if (word exists for subset)
				recurse (occurances subtract subset)
	
	def innerFunc(argument) {
		...
		for {
		  branchingChoice <- enumerateBranchingChoices(argument)
		  subSolution <- innerFunc(reducedArgument(argument, branchingChoice))
		  if isValid(subSolution, branchingChoice)
		} yield someComposition(subSolution, branchingChoice)
		... or...
		for {
		  subset <- combinations(occurances)
		  subSolution <- innerFunc(subtract(occurances, subset))
		  if isValid(subSolution, branchingChoice)
		} yield someComposition(subSolution, branchingChoice)
	}

	The algo that I use is the following:
		- for the given sentence figure out the occurrence list and the combinations [DONE]

		- Next using foldLeft figure out all the sentences (using a recursive sub routine called anagrams for a single occurrence)
			for each subset and add them to the list to return

		- anagrams for a single occurrence does the following
				- Figure out words which match the occurrence, if found proceed further else return Nil
				- If words are found, then figure out remaining occurrences using subtract,
					similarly figure out the remaining combinations using subtract (I pass along the original combinations list)
				- Recursively call the top level function to get all sentences for the remaining occurrence.
			
	def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def sent_helper(sen_occ: Occurrences): List[List[Word]] =
      if (sen_occ.isEmpty) List(List())
      else
        for {
          occur <- combinations(sen_occ)
          word <- dictionaryByOccurrences(occur)
          rest <- sent_helper(subtract(sen_occ, occur))
        } yield word :: rest
        
    sent_helper(sentenceOccurrences(sentence))
  }
	*/
}