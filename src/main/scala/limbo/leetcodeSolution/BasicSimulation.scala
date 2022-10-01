package limbo.leetcodeSolution

import limbo.leetcodeSolution.Solution.ListNode

object BasicSimulation {

  //263. Ugly Number
  /*
    check whether a given number is an ugly number.

    Ugly numbers are positive numbers whose prime factors only include 2, 3, 5.

    Example:

    Input: 6
    Output: true
    Explanation: 6 = 2 × 3

    Input: 14
    Output: false
    Explanation: 14 is not ugly since it includes another prime factor 7.

    1 is typically treated as an ugly number.
    Input is within the 32-bit signed integer range: [−231,  231 − 1].

  */
  def isUgly(num: Int): Boolean = {
    val arr = Array(2,3,5)
    var n = num
    for(prime <- arr){
      while(n > 0 && n % prime == 0)
        n /= prime
    }

    n == 1
  }


  //67. Add Binary
  /*
    Given two binary strings, return their sum (also a binary string).

    The input strings are both non-empty and contains only characters 1 or 0.

    Example :

    Input: a = "11", b = "1"
    Output: "100"

    可以不用reverse，从长度较长的那个String长度开始，从后往前

  */
  def addBinary(a: String, b: String): String = {
    val ra = a.reverse.toStream
    val rb = b.reverse.toStream
    val zipAB = ra.zipAll(rb,'0','0')
    var carry = 0
    var res = zipAB.foldLeft("")((x, y) => {
      val a = y._1 - '0'
      val b = y._2 - '0'
      var s = a + b + carry
      carry = s / 2
      s = s % 2

      s.toString + x
    })

    if(carry == 1)
      res = '1' + res

    res

//    val ra = a.reverse
//    val rb = b.reverse
//    var res = ""
//    var carry = 0
//    for(i <- 0 to Integer.MAX_VALUE
//        if i < ra.length || i < rb.length){
//      val ca = if(i >= ra.length) '0' else ra(i) - '0'
//      val cb = if(i >= rb.length) '0' else rb(i) - '0'
//      var s = ca + cb + carry
//      carry = s / 2
//      s = s % 2
//      res = s.toChar +: res
//    }
//
//    //extra carry out of range
//    if(carry == 1)
//      res = '1' +: res
//
//    res
  }


  //504. Base 7
  /*
    Given an integer, return its base 7 string representation.

    Example 1:
    Input: 100
    Output: "202"
    Example 2:
    Input: -7
    Output: "-10"
    Note: The input will be in range of [-1e7, 1e7].

  */
  def convertToBase7(num: Int): String = {
    if(num == 0)
      return "0"

    var res = ""
    var n = num
    var is_neg = false
    if(num < 0){
      n = -1 * num
      is_neg = true
    }
    while(n > 0){
      val cur = n % 7
      res = cur.toString + res
      n = n / 7
    }

    if(is_neg)
      "-" + res
    else
      res
  }


  //54. Spiral Matrix
  /*
    Input:
    [
     [ 1, 2, 3 ],
     [ 4, 5, 6 ],
     [ 7, 8, 9 ]
    ]
    Output: [1,2,3,6,9,8,7,4,5]

  */
  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
    var listBuffer = scala.collection.mutable.ListBuffer[Int]()
    if(matrix == null || matrix.length == 0)
      return List()

    var rows = matrix.length
    var cols = matrix(0).length
    var ceiling = 0
    var floor = rows - 1
    var leftMost = 0
    var rightMost = cols - 1

    while(listBuffer.length < rows * cols){
      //be care of turning point
      //go right
      for(j <- leftMost to rightMost if listBuffer.length < rows * cols)
        listBuffer += matrix(ceiling)(j)
      //go down
      for(i <- ceiling + 1 to floor if listBuffer.length < rows * cols)
        listBuffer += matrix(i)(rightMost)
      //go left
      for(j <- rightMost - 1 to leftMost by -1 if listBuffer.length < rows * cols)
        listBuffer += matrix(floor)(j)
      //go up
      for(i <- floor - 1 to ceiling + 1 by -1 if listBuffer.length < rows * cols)
        listBuffer += matrix(i)(leftMost)

      //shrinking the square
      ceiling += 1
      floor -= 1
      leftMost += 1
      rightMost -= 1
    }

    listBuffer.toList
  }


  //24. Swap Nodes in Pairs
  /*
    Given a linked list, swap every two adjacent nodes and return its head.

    You may not modify the values in the list's nodes, only nodes itself may be changed.



    Example:

    Given 1->2->3->4, you should return the list as 2->1->4->3.

    dummy -> 1 -> 2 -> 3 -> 4 -> null
      p      a    b

    p.next = b
    a.next = b.next
    b.next = a
    p = a


    dummy -> 2 -> 1 -> 3 -> 4 -> null

                  p    a    b


  */
  def swapPairs(head: ListNode): ListNode = {
    val dummy = new ListNode()
    var p = new ListNode()
    dummy.next = head
    p = dummy
    //iterate p
    while(p != null && p.next != null && p.next.next != null){
      val a = p.next
      val b = p.next.next
      p.next = b
      a.next = b.next
      b.next = a
      p = a
    }

    dummy.next
  }


  //299. Bulls and Cows
  /*
    You are playing the following Bulls and Cows game with your friend: You write down a number and ask your friend to guess what the number is. Each time your friend makes a guess, you provide a hint that indicates how many digits in said guess match your secret number exactly in both digit and position (called "bulls") and how many digits match the secret number but locate in the wrong position (called "cows"). Your friend will use successive guesses and hints to eventually derive the secret number.

    Write a function to return a hint according to the secret number and friend's guess, use A to indicate the bulls and B to indicate the cows.

    Please note that both secret number and friend's guess may contain duplicate digits.

    Example 1:

    Input: secret = "1807", guess = "7810"

    Output: "1A3B"

    Explanation: 1 bull and 3 cows. The bull is 8, the cows are 0, 1 and 7.
    Example 2:

    Input: secret = "1123", guess = "0111"

    Output: "1A1B"

    Explanation: The 1st 1 in friend's guess is a bull, the 2nd or 3rd 1 is a cow.

    Note: You may assume that the secret number and your friend's guess only contain digits, and their lengths are always equal.
  */
  def getHint(secret: String, guess: String): String = {
    /*
      val cs = Array.fill(10)(0)
      val cg = Array.fill(10)(0)
      separately recording the number of occurrences of 0 ~ 9

      var sum += min(cs[i],cg[i])
      val a = check every digit from beginning by index
      val b = sum - a

    */
    val cs = Array.fill(10)(0)
    val cg = Array.fill(10)(0)
    var sum = 0
    var a = 0

    for(i <- 0 until secret.length){
      cs(secret(i) - '0') += 1
      cg(guess(i) - '0') += 1

      if(secret(i) == guess(i))
        a += 1
    }

    //count the same number of secret and guess
    for(j <- 0 to 9){
      sum += Math.min(cs(j),cg(j))
    }

    a + "A" + (sum - a) + "B"
  }


  //481. Magical String
  /*
    A magical string S consists of only '1' and '2' and obeys the following rules:

    The string S is magical because concatenating the number of contiguous occurrences of characters '1' and '2' generates the string S itself.

    The first few elements of string S is the following: S = "1221121221221121122……"

    If we group the consecutive '1's and '2's in S, it will be:

    1 22 11 2 1 22 1 22 11 2 11 22 ......

    and the occurrences of '1's or '2's in each group are:

    1 2 2 1 1 2 1 2 2 1 2 2 ......

    You can see that the occurrence sequence above is the S itself.

    Given an integer N as input, return the number of '1's in the first N number in the magical string S.

    Note: N will not exceed 100,000.

    Example 1:
    Input: 6
    Output: 3
    Explanation: The first 6 elements of magical string S is "12211"
    and it contains three 1's, so return 3.

  */
  def magicalString(n: Int): Int = {
    var s = "122"
    //122.. -> 1 22 11
    for(i <- 2 until n){
      var nextDigit = 1
      // s[i] - '0' represent how many times nextDigit should be appended
      for(j <- 0 to Integer.MAX_VALUE if j < s(i) - '0')
        s += nextDigit.toString
      nextDigit = 3 - nextDigit
    }

    var res = 0
    //counting 1s
    for(i <- 0 until n){
      if(s(i) == '1')
        res += 1
    }

    res
  }


  //71. Simplify Path
  /*
    Given an absolute path for a file (Unix-style), simplify it. Or in other words, convert it to the canonical path.

    In a UNIX-style file system, a period . refers to the current directory. Furthermore, a double period .. moves the directory up a level. For more information, see: Absolute path vs relative path in Linux/Unix

    Note that the returned canonical path must always begin with a slash /, and there must be only a single slash / between two directory names. The last directory name (if it exists) must not end with a trailing /. Also, the canonical path must be the shortest string representing the absolute path.


  */
  def simplifyPath(path: String): String = {
    //in case of path has no '/' end.
    //seg needs '/' for indicating
    var _path = path + "/"
    var res,seg = ""
    for(c <- _path){
      if(res == "")
        res += c
      // append chars to seg until c == '/'
      else if(c != '/')
        seg += c
      else{
        //dual with ..
        //get rid of previous level
        //be care of res + seg == "/.."
        if(seg == ".."){
          //remove last / in previous segment
          if(res != "/")
            res = res.slice(0,res.length - 1)
          //remove chars of previous segment
          while(res.last != '/')
            res = res.slice(0,res.length - 1)
        }
        // in case of seg == '.' or seg likes '//////'
        else if(seg != "." && seg != "")
          res = res + seg + "/"

        //reset seg to ""
        seg = ""
      }
    }

    //remove the '/' that last iteration appended
    if(res.length > 1)
      res = res.slice(0,res.length - 1)

    res
  }


  //12. Integer to Roman
  /*
    Roman numerals are represented by seven different symbols: I, V, X, L, C, D and M.

    Symbol       Value
    I             1
    V             5
    X             10
    L             50
    C             100
    D             500
    M             1000
    For example, two is written as II in Roman numeral, just two one's added together. Twelve is written as, XII, which is simply X + II. The number twenty seven is written as XXVII, which is XX + V + II.

    Roman numerals are usually written largest to smallest from left to right. However, the numeral for four is not IIII. Instead, the number four is written as IV. Because the one is before the five we subtract it making four. The same principle applies to the number nine, which is written as IX. There are six instances where subtraction is used:

    I can be placed before V (5) and X (10) to make 4 and 9.
    X can be placed before L (50) and C (100) to make 40 and 90.
    C can be placed before D (500) and M (1000) to make 400 and 900.
    Given an integer, convert it to a roman numeral. Input is guaranteed to be within the range from 1 to 3999.


  */
  def intToRoman(num: Int): String = {
    var _num = num
    val intStream = Array(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
    val roman = Array("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I")
    var res = ""
    var i = 0
    while (_num > 0) {
      while (_num >= intStream(i)) {
        res = res + roman(i)
        _num = _num - intStream(i)
      }
      i = i + 1
    }

    res
  }


  //68. Text Justification
  /*
    Given an array of words and a width maxWidth, format the text such that each line has exactly maxWidth characters and is fully (left and right) justified.

    You should pack your words in a greedy approach; that is, pack as many words as you can in each line. Pad extra spaces ' ' when necessary so that each line has exactly maxWidth characters.

    Extra spaces between words should be distributed as evenly as possible.
    If the number of spaces on a line do not divide evenly between words,
    the empty slots on the left will be assigned more spaces than the slots on the right.

    For the last line of text, it should be left justified and no extra space is inserted between words.

    Note:

    A word is defined as a character sequence consisting of non-space characters only.
    Each word's length is guaranteed to be greater than 0 and not exceed maxWidth.
    The input array words contains at least one word.

    一行一行处理，每次先求出这一行最多可以放多少个单词，然后分三种情况处理：

    如果是最后一行，则只实现左对齐：每个单词之间插入一个空格，行尾插入若干空格，使这一行的总长度是 maxWidth；
    如果这一行只有一个单词，则直接在行尾补上空格；
    其他情况，则需计算总共要填补多少空格，然后按题意均分在单词之间；

    line by line, get the number of how many words can be fill in one line at most

    if current line is the bottom, left justify , fill the remain with space till maxWidth
    if current line just have one word , fill the remain with space till maxWidth
    otherwise , counting the spaces that should be filled ,then distribute as evenly as possible

    i                j
    -----maxWidth-----




  */
  def fullJustify(words: Array[String], maxWidth: Int): List[String] = {

    def printSpace(x:Int):String = {
      var res = ""
      var _x = x
      while(_x > 0){
        res += ' '
        _x -= 1
      }

      res
    }

    val res = scala.collection.mutable.ListBuffer[String]()
    var i = 0
    while(i < words.length){
      var j = i + 1
      var containSpace = words(i).length
      var withoutSpace = words(i).length
      while(j < words.length && containSpace + 1 + words(j).length <= maxWidth){
        containSpace += words(j).length + 1
        withoutSpace += words(j).length
        j += 1
      }
      val spaces = maxWidth - withoutSpace
      var line = words(i)
      //current line is bottom
      if(j == words.length){
        for(k <- i + 1 to Integer.MAX_VALUE if k < j)
          line = line + ' ' + words(k)
        line += printSpace(maxWidth - line.length)
      }
      // current line just one word
      else if(j - i == 1){
        line += printSpace(maxWidth - line.length)
      }
      // as evenly as possible
      // j - i - 1 represent the number of words in line but except the first word
      // base means the number of spaces that one of the "words" can get
      // rem should be assigned to the left ones than the "words" on the right.
      else{
        val base = spaces / (j - i - 1)
        val rem = spaces % (j - i - 1)
        var k = 0
        // note the current j has already gone to next line
        for(index <- i + 1 until j){
          if(k < rem){
            line = line + printSpace(base + 1) + words(index)
            k += 1
          }
          else
            line = line + printSpace(base) + words(index)
        }
      }
      i = j
      res += line
    }

    res.toList
  }






}
