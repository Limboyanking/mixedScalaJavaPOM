package limbo.leetcodeSolution

import scala.collection.mutable

object TwoPointers {
  //3. Longest Substring Without Repeating Characters
  /*
    Given a string, find the length of the longest substring without repeating characters.

    Example 1:

    Input: "abcabcbb"
    Output: 3
    Explanation: The answer is "abc", with the length of 3.
    Example 2:

    Input: "bbbbb"
    Output: 1
    Explanation: The answer is "b", with the length of 1.
    Example 3:

    Input: "pwwkew"
    Output: 3
    Explanation: The answer is "wke", with the length of 3.
                 Note that the answer must be a substring, "pwke" is a subsequence and not a substring.

  */
  def lengthOfLongestSubstring(s: String): Int = {
    if(s == "")
      return 0

    var res = 0
    var start = 0
    val record = mutable.Map[Char,Int]()
    for(i <- 0 until s.length){
      if(record.contains(s(i)))
        start = math.max(start,record(s(i)) + 1)

      record += (s(i) -> i)
      res = math.max(res,i - start + 1)
    }

    res
  }


}
