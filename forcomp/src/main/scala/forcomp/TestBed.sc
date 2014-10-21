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
  	(w.toLowerCase().toList
  		groupBy (s => s)
  		map { case (x, y) => (x, y.length) }
  		toList) sorted                    //> wordOccurrences: (w: forcomp.TestBed.Word)forcomp.TestBed.Occurrences

  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)
                                                  //> sentenceOccurrences: (s: forcomp.TestBed.Sentence)forcomp.TestBed.Occurrence
                                                  //| s
	val test = List("ate", "tea", "joe")      //> test  : List[String] = List(ate, tea, joe)
  val grouped = test map (w => wordOccurrences(w) -> w) groupBy {
  	case (x, y) => x
  } map {
  	case (x, y) => x -> (y map (p => p._2 ))
  }                                               //> grouped  : scala.collection.immutable.Map[forcomp.TestBed.Occurrences,List[S
                                                  //| tring]] = Map(List((e,1), (j,1), (o,1)) -> List(joe), List((a,1), (e,1), (t,
                                                  //| 1)) -> List(ate, tea))
  
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    test//dictionary
      .map(w => wordOccurrences(w) -> w)
      .groupBy { case (occur, word) => occur }
      .map { case (occur, grouping) => occur -> (grouping map { case (o, word) => word }) }
      .withDefaultValue(Nil)                      //> dictionaryByOccurrences: => Map[forcomp.TestBed.Occurrences,List[forcomp.Tes
                                                  //| tBed.Word]]
      
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))
                                                  //> wordAnagrams: (word: forcomp.TestBed.Word)List[forcomp.TestBed.Word]

	wordAnagrams("married")                   //> res0: List[forcomp.TestBed.Word] = List()
  dictionaryByOccurrences take 1                  //> res1: scala.collection.immutable.Map[forcomp.TestBed.Occurrences,List[forco
                                                  //| mp.TestBed.Word]] = Map(List((e,1), (j,1), (o,1)) -> List(joe))
  wordOccurrences("Aberdeen")                     //> res2: forcomp.TestBed.Occurrences = List((a,1), (b,1), (d,1), (e,3), (n,1),
                                                  //|  (r,1))
	sentenceOccurrences(List("Aberdeen", "Abernathy")  )
                                                  //> res3: forcomp.TestBed.Occurrences = List((a,3), (b,2), (d,1), (e,4), (h,1),
                                                  //|  (n,2), (r,2), (t,1), (y,1))
}