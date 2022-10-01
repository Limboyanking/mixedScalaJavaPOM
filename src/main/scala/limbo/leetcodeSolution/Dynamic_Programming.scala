package limbo.leetcodeSolution

object Dynamic_Programming {
  //120. Triangle
  /*
    Given a triangle, find the minimum path sum from top to bottom. Each step you may move to adjacent numbers on the row below.

    For example, given the following triangle

    [
         [2],
        [3,4],
       [6,5,7],
      [4,1,8,3]
    ]
    The minimum path sum from top to bottom is 11 (i.e., 2 + 3 + 5 + 1 = 11).

    Note:

    Bonus point if you are able to do this using only O(n) extra space, where n is the total number of rows in the triangle.

  */
  def minimumTotal(triangle: List[List[Int]]): Int = {

    /*
      //since the row minpath[k+1] would be useless
      //after minpath[k] is computed, we can simply set minpath as a 1D array, and iteratively update itself:
      int n = triangle.size();
      vector<int> minlen(triangle.back());
      for (int layer = n-2; layer >= 0; layer--) // For each layer
      {
          for (int i = 0; i <= layer; i++) // Check its every 'node'
          {
              // For the kth level, find the lesser of its two children, and sum the current value in the triangle with it.
              minlen[i] = min(minlen[i], minlen[i+1]) + triangle[layer][i];
          }
      }
      return minlen[0];
    */

    /**
      * 2
      * *
      * 3  4
      * *
      * 6  5  7
      * *
      * 4  1  8  3
      * *
      * opt(i,j)    i 代指 第i列  j 代指 第选择j个
      * *
      * opt(3,0) = Min(     opt(2,0)      ,     opt(2,1)    ) + 4
      */

    val opt = scala.collection.mutable.Map[(Int, Int), Int]()
    var ret = Integer.MAX_VALUE

    if (triangle.length < 1)
      return 0

    for (i <- triangle.indices) {
      for (j <- triangle(i).indices) {
        if (i == 0)
          opt += ((0, 0) -> triangle(i)(j))
        else if (j < i - 1)
          opt += ((i, j) -> (Math.min(opt((i - 1, j)), opt(i - 1, j + 1)) + triangle(i)(j)))
        else if (j == i - 1)
          opt += ((i, j) -> (if (i - 1 == 0) opt((0, 0)) + triangle(i)(j)
          else Math.min(opt((i - 1, j)), opt(i - 1, j - 1)) + triangle(i)(j)))
        else if (j > i - 1)
          opt += ((i, j) -> (opt((i - 1, j - 1)) + triangle(i)(j)))
        if (i == triangle.length - 1)
          ret = Math.min(ret, opt((i, j)))
      }
    }

    ret
  }

  //63. Unique Paths II
  /*
    A robot is located at the top-left corner of a m x n grid (marked 'Start' in the diagram below).

    The robot can only move either down or right at any point in time. The robot is trying to reach the bottom-right corner of the grid (marked 'Finish' in the diagram below).

    Now consider if some obstacles are added to the grids. How many unique paths would there be?

    An obstacle and empty space is marked as 1 and 0 respectively in the grid.

    Note: m and n will be at most 100.

    Example 1:

    Input:
    [
      [0,0,0],
      [0,1,0],
      [0,0,0]
    ]
    Output: 2
    Explanation:
    There is one obstacle in the middle of the 3x3 grid above.
    There are two ways to reach the bottom-right corner:
    1. Right -> Right -> Down -> Down
    2. Down -> Down -> Right -> Right

  */
  def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int = {

    0
  }

  //354. Russian Doll Envelopes
  /*
    You have a number of envelopes with widths and heights given as a pair of integers (w, h). One envelope can fit into another if and only if both the width and height of one envelope is greater than the width and height of the other envelope.

    What is the maximum number of envelopes can you Russian doll? (put one inside other)

    Note:
    Rotation is not allowed.

    Example:

    Input: [[5,4],[6,4],[6,7],[2,3]]
    Output: 3
    Explanation: The maximum number of envelopes you can Russian doll is 3 ([2,3] => [5,4] => [6,7]).

  */
  def maxEnvelopes(envelopes: Array[Array[Int]]): Int = {

    0
  }

  //338. Counting Bits
  /*
    Given a non negative integer number num. For every numbers i in the range 0 ≤ i ≤ num calculate the number of 1's in their binary representation and return them as an array.

    Example 1:

    Input: 2
    Output: [0,1,1]
    Example 2:

    Input: 5
    Output: [0,1,1,2,1,2]
    Follow up:

    It is very easy to come up with a solution with run time O(n*sizeof(integer)). But can you do it in linear time O(n) /possibly in a single pass?
    Space complexity should be O(n).
    Can you do it like a boss? Do it without using any builtin function like __builtin_popcount in c++ or in any other language.

  */
  def countBits(num: Int): Array[Int] = {
    Array()
  }

  //329. Longest Increasing Path in a Matrix
  /*
    Given an integer matrix, find the length of the longest increasing path.

    From each cell, you can either move to four directions: left, right, up or down. You may NOT move diagonally or move outside of the boundary (i.e. wrap-around is not allowed).

    Example 1:

    Input: nums =
    [
      [9,9,4],
      [6,6,8],
      [2,1,1]
    ]
    Output: 4
    Explanation: The longest increasing path is [1, 2, 6, 9].
    Example 2:

    Input: nums =
    [
      [3,4,5],
      [3,2,6],
      [2,2,1]
    ]
    Output: 4
    Explanation: The longest increasing path is [3, 4, 5, 6]. Moving diagonally is not allowed.

  */
  def longestIncreasingPath(matrix: Array[Array[Int]]): Int = {
    0
  }

  //322. Coin Change
  /*
    You are given coins of different denominations and a total amount of money amount. Write a function to compute the fewest number of coins that you need to make up that amount. If that amount of money cannot be made up by any combination of the coins, return -1.

    Example 1:

    Input: coins = [1, 2, 5], amount = 11
    Output: 3
    Explanation: 11 = 5 + 5 + 1
    Example 2:

    Input: coins = [2], amount = 3
    Output: -1
    Note:
    You may assume that you have an infinite number of each kind of coin.

  */
  def coinChange(coins: Array[Int], amount: Int): Int = {
    0
  }

  //221. Maximal Square
  /*
    Given a 2D binary matrix filled with 0's and 1's, find the largest square containing only 1's and return its area.

    Example:

    Input:

    1 0 1 0 0
    1 0 1 1 1
    1 1 1 1 1
    1 0 0 1 0

    Output: 4
  */
  def maximalSquare(matrix: Array[Array[Char]]): Int = {
    0
  }

  //576. Out of Boundary Paths
  /*
    There is an m by n grid with a ball. Given the start coordinate (i,j) of the ball, you can move the ball to adjacent cell or cross the grid boundary in four directions (up, down, left, right). However, you can at most move N times. Find out the number of paths to move the ball out of grid boundary. The answer may be very large, return it after mod 109 + 7.
  */
  def findPaths(m: Int, n: Int, N: Int, i: Int, j: Int): Int = {
    0
  }

  //91. Decode Ways
  /*
    A message containing letters from A-Z is being encoded to numbers using the following mapping:

    'A' -> 1
    'B' -> 2
    ...
    'Z' -> 26
    Given a non-empty string containing only digits, determine the total number of ways to decode it.

    Example 1:

    Input: "12"
    Output: 2
    Explanation: It could be decoded as "AB" (1 2) or "L" (12).
    Example 2:

    Input: "226"
    Output: 3
    Explanation: It could be decoded as "BZ" (2 26), "VF" (22 6), or "BBF" (2 2 6).

  */
  def numDecodings(s: String): Int = {
    0
  }

  //264. Ugly Number II
  /*
    Write a program to find the n-th ugly number.

    Ugly numbers are positive numbers whose prime factors only include 2, 3, 5.

    Example:

    Input: n = 10
    Output: 12
    Explanation: 1, 2, 3, 4, 5, 6, 8, 9, 10, 12 is the sequence of the first 10 ugly numbers.
    Note:

    1 is typically treated as an ugly number.
    n does not exceed 1690.

  */
  def nthUglyNumber(n: Int): Int = {
    0
  }

  //115. Distinct Subsequences
  /*
    Given a string S and a string T, count the number of distinct subsequences of S which equals T.

    A subsequence of a string is a new string which is formed from the original string by deleting some (can be none) of the characters without disturbing the relative positions of the remaining characters. (ie, "ACE" is a subsequence of "ABCDE" while "AEC" is not).

    It's guaranteed the answer fits on a 32-bit signed integer.

    Example 1:

    Input: S = "rabbbit", T = "rabbit"
    Output: 3
    Explanation:
    As shown below, there are 3 ways you can generate "rabbit" from S.
    (The caret symbol ^ means the chosen letters)

    rabbbit
    ^^^^ ^^
    rabbbit
    ^^ ^^^^
    rabbbit
    ^^^ ^^^
    Example 2:

    Input: S = "babgbag", T = "bag"
    Output: 5
    Explanation:
    As shown below, there are 5 ways you can generate "bag" from S.
    (The caret symbol ^ means the chosen letters)

    babgbag
    ^^ ^
    babgbag
    ^^    ^
    babgbag
    ^    ^^
    babgbag
      ^  ^^
    babgbag
        ^^^
  */
  def numDistinct(s: String, t: String): Int = {
    0
  }

  //132. Palindrome Partitioning II
  /*
    Given a string s, partition s such that every substring of the partition is a palindrome.

    Return the minimum cuts needed for a palindrome partitioning of s.

    Example:

    Input: "aab"
    Output: 1
    Explanation: The palindrome partitioning ["aa","b"] could be produced using 1 cut.
  */
  def minCut(s: String): Int = {
    0
  }

  //526. Beautiful Arrangement
  /*
    Suppose you have N integers from 1 to N. We define a beautiful arrangement as an array that is constructed by these N numbers successfully if one of the following is true for the ith position (1 <= i <= N) in this array:

    The number at the ith position is divisible by i.
    i is divisible by the number at the ith position.


    Now given N, how many beautiful arrangements can you construct?

    Example 1:

    Input: 2
    Output: 2
    Explanation:

    The first beautiful arrangement is [1, 2]:

    Number at the 1st position (i=1) is 1, and 1 is divisible by i (i=1).

    Number at the 2nd position (i=2) is 2, and 2 is divisible by i (i=2).

    The second beautiful arrangement is [2, 1]:

    Number at the 1st position (i=1) is 2, and 2 is divisible by i (i=1).

    Number at the 2nd position (i=2) is 1, and i (i=2) is divisible by 1.


    Note:

    N is a positive integer and will not exceed 15.

  */
  def countArrangement(N: Int): Int = {
    0
  }

  //486. Predict the Winner
  /*
    Given an array of scores that are non-negative integers. Player 1 picks one of the numbers from either end of the array followed by the player 2 and then player 1 and so on. Each time a player picks a number, that number will not be available for the next player. This continues until all the scores have been chosen. The player with the maximum score wins.

    Given an array of scores, predict whether player 1 is the winner. You can assume each player plays to maximize his score.

    Example 1:
    Input: [1, 5, 2]
    Output: False
    Explanation: Initially, player 1 can choose between 1 and 2.
    If he chooses 2 (or 1), then player 2 can choose from 1 (or 2) and 5. If player 2 chooses 5, then player 1 will be left with 1 (or 2).
    So, final score of player 1 is 1 + 2 = 3, and player 2 is 5.
    Hence, player 1 will never be the winner and you need to return False.
    Example 2:
    Input: [1, 5, 233, 7]
    Output: True
    Explanation: Player 1 first chooses 1. Then player 2 have to choose between 5 and 7. No matter which number player 2 choose, player 1 can choose 233.
    Finally, player 1 has more score (234) than player 2 (12), so you need to return True representing player1 can win.
    Note:
    1 <= length of the array <= 20.
    Any scores in the given array are non-negative integers and will not exceed 10,000,000.
    If the scores of both players are equal, then player 1 is still the winner.
  */
  def PredictTheWinner(nums: Array[Int]): Boolean = {
    false
  }















}
