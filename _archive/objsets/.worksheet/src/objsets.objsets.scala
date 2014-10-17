package objsets

object objsets {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(147); 
  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  };System.out.println("""asSet: (tweets: objsets.TweetSet)Set[objsets.Tweet]""");$skip(50); 

  def size(set: TweetSet): Int = asSet(set).size;System.out.println("""size: (set: objsets.TweetSet)Int""");$skip(24); 

  val set1 = new Empty;System.out.println("""set1  : objsets.Empty = """ + $show(set1 ));$skip(53); 
  val set2 = set1.incl(new Tweet("a", "a body", 20));System.out.println("""set2  : objsets.TweetSet = """ + $show(set2 ));$skip(39); 
  val c = new Tweet("c", "c body", 37);System.out.println("""c  : objsets.Tweet = """ + $show(c ));$skip(39); 
  val d = new Tweet("d", "d body", 21);System.out.println("""d  : objsets.Tweet = """ + $show(d ));$skip(27); 
  val set5a = set2.incl(c);System.out.println("""set5a  : objsets.TweetSet = """ + $show(set5a ));$skip(28); 
  val set5b = set5a.incl(d);System.out.println("""set5b  : objsets.TweetSet = """ + $show(set5b ));$skip(32); 

  println(set5b.mostRetweeted);$skip(40); 
  println(set5b.mostRetweeted.retweets);$skip(45); 

  set5b.descendingByRetweet foreach println;$skip(80); 
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus");System.out.println("""google  : List[String] = """ + $show(google ));$skip(72); 
  
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad");System.out.println("""apple  : List[String] = """ + $show(apple ));$skip(104); 
  val appleTweets =
  	TweetReader.allTweets.filter((x) => apple.exists((str) => x.text.contains(str)));System.out.println("""appleTweets  : objsets.TweetSet = """ + $show(appleTweets ));$skip(106); 
  val googleTweets =
  	TweetReader.allTweets.filter((x) => google.exists((str) => x.text.contains(str)));System.out.println("""googleTweets  : objsets.TweetSet = """ + $show(googleTweets ));$skip(40); val res$0 = 
    
    
  size(TweetReader.allTweets);System.out.println("""res0: Int = """ + $show(res$0));$skip(21); val res$1 = 
  size(googleTweets);System.out.println("""res1: Int = """ + $show(res$1));$skip(20); val res$2 = 
  size(appleTweets);System.out.println("""res2: Int = """ + $show(res$2))}
}
