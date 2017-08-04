package calculator

object TweetLength {
  final val MaxTweetLength = 140

  // `remainingChars` tracks the number of chars left in a tweet.
  val remainingChars: Var[Int] = Var(MaxTweetLength)

  // Compute the remaining characters when the input signal `tweetText` changes, and update our
  // output signal `remainingChars`.
  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    remainingChars() = MaxTweetLength - tweetLength(tweetText())
    remainingChars
  }

  // If we don't have our output signal follow the input signal, we fail a submission test.
  // So, my color code signal is Variable, and I will update it based on input signal changes.
  val colorCode: Var[String] = Var(colorCodeTheCount(MaxTweetLength))

  // Abstracts the count -> color code translation since both initializing colorSignal and updating
  // it need the same code.  Do it once....
  private def colorCodeTheCount(count: Int): String = {
    if      (count >= 15) "green"
    else if (count >= 0)  "orange"
    else                  "red"
  }

  // Based on the input signal `remainingCharsCount`, determine the appropriate color code
  // and update our output signal `colorCode`.
  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Var[String] = {
    colorCode() = colorCodeTheCount(remainingCharsCount())
    colorCode
  }

  /** Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as tweet lengths count the number
   *  of Unicode *code points* in the string.
   *  Note that this is still a simplified view of the reality. Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
          (Character.isSurrogatePair _).tupled)
    }
  }
}
