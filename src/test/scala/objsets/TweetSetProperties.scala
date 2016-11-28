package objsets

import org.scalacheck.{Arbitrary, Gen, Properties, Prop, Shrink}
import Arbitrary.arbitrary
import Shrink.shrink
import Prop.forAll


/**
  * Property checks for [[TweetSet]].
  */
object TweetSetProperties extends Properties("TweetSet") {

  def genTweet(userGen : Gen[String], textGen : Gen[String], retweetsGen : Gen[Int]) : Gen[Tweet] =
    for {
      user <- userGen
      text <- textGen
      retweets <- retweetsGen
    } yield new Tweet(user, text, retweets)

  // Note that for this to realistic, both the username and text should have bounded lengths.
  // But the Tweet classes doesn't care, and neither do we.
  implicit val arbTweet =
    Arbitrary(genTweet(arbitrary[String], arbitrary[String], Gen.chooseNum(0, Int.MaxValue)))

  implicit val shrinkTweet = Shrink {tweet : Tweet =>
    for {
      user <- shrink(tweet.user)
      text <- shrink(tweet.text)
      retweets <- shrink(tweet.retweets)
    } yield new Tweet(user, text, retweets)
  }


  property("union results in a TweetSet that contains all tweets in either set") =
    forAll {(tweets1 : Seq[Tweet], tweets2 : Seq[Tweet]) =>
      // We generate the TweetSets by generating sequences of Tweets to insert one at a time.
      val tweetSet1 = ((new Empty : TweetSet) /: tweets1) {(ts, t) => ts.incl(t)}
      val tweetSet2 = ((new Empty : TweetSet) /: tweets2) {(ts, t) => ts.incl(t)}
      val unionSet = tweetSet1 union tweetSet2
      tweets1 ++ tweets2 forall unionSet.contains
    }

}
