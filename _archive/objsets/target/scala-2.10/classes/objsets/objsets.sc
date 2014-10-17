package objsets

object objsets {
  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }                                               //> asSet: (tweets: objsets.TweetSet)Set[objsets.Tweet]

  def size(set: TweetSet): Int = asSet(set).size  //> size: (set: objsets.TweetSet)Int

  val set1 = new Empty                            //> set1  : objsets.Empty = objsets.Empty@3936d1b4
  val set2 = set1.incl(new Tweet("a", "a body", 20))
                                                  //> set2  : objsets.TweetSet = objsets.NonEmpty@8262adc
  val c = new Tweet("c", "c body", 37)            //> c  : objsets.Tweet = User: c
                                                  //| Text: c body [37]
  val d = new Tweet("d", "d body", 21)            //> d  : objsets.Tweet = User: d
                                                  //| Text: d body [21]
  val set5a = set2.incl(c)                        //> set5a  : objsets.TweetSet = objsets.NonEmpty@3041143e
  val set5b = set5a.incl(d)                       //> set5b  : objsets.TweetSet = objsets.NonEmpty@980b32

  println(set5b.mostRetweeted)                    //> User: c
                                                  //| Text: c body [37]
  println(set5b.mostRetweeted.retweets)           //> 37

  set5b.descendingByRetweet foreach println       //> User: c
                                                  //| Text: c body [37]
                                                  //| User: d
                                                  //| Text: d body [21]
                                                  //| User: a
                                                  //| Text: a body [20]
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
                                                  //> google  : List[String] = List(android, Android, galaxy, Galaxy, nexus, Nexus
                                                  //| )
  
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
                                                  //> apple  : List[String] = List(ios, iOS, iphone, iPhone, ipad, iPad)
  val appleTweets =
  	TweetReader.allTweets.filter((x) => apple.exists((str) => x.text.contains(str)))
                                                  //> appleTweets  : objsets.TweetSet = objsets.NonEmpty@5c2c48a6
  val googleTweets =
  	TweetReader.allTweets.filter((x) => google.exists((str) => x.text.contains(str)))
                                                  //> googleTweets  : objsets.TweetSet = objsets.NonEmpty@2888bd33
    
    
  size(TweetReader.allTweets)                     //> res0: Int = 695
  size(googleTweets)                              //> res1: Int = 38
  size(appleTweets)                               //> res2: Int = 150
}