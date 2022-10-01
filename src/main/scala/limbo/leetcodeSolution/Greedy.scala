package limbo.leetcodeSolution

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Greedy {
  //860. Lemonade Change
  /*
    At a lemonade stand, each lemonade costs $5.

    Customers are standing in a queue to buy from you, and order one at a time (in the order specified by bills).

    Each customer will only buy one lemonade and pay with either a $5, $10, or $20 bill.  You must provide the correct change to each customer, so that the net transaction is that the customer pays $5.

    Note that you don't have any change in hand at first.

    Return true if and only if you can provide every customer with correct change.

    Input: [5,5,5,10,20]
    Output: true
    Explanation:
    From the first 3 customers, we collect three $5 bills in order.
    From the fourth customer, we collect a $10 bill and give back a $5.
    From the fifth customer, we give a $10 bill and a $5 bill.
    Since all customers got correct change, we output true.
  */
  def lemonadeChange(bills: Array[Int]): Boolean = {
    // 5$ 比 10$ 金贵
    var fives = 0
    var tens = 0
    for (b <- bills) {
      if (b == 5)
        fives += 1
      else if (b == 10) {
        if (fives > 0) {
          fives -= 1
          tens += 1
        }
        else
          return false
      }
      else {
        //change needs to be 15
        // 10 + 5 or 5 + 5 + 5
        var change = 15

        if (tens > 0) {
          tens -= 1
          change -= 10
        }
        while (change > 0 && fives > 0) {
          change -= 5
          fives -= 1
        }

        if (change > 0)
          return false
      }
    }

    true
  }

  //392. Is Subsequence
  /*
    Given a string s and a string t, check if s is subsequence of t.

    You may assume that there is only lower case English letters in both s and t.
    t is potentially a very long (length ~= 500,000) string, and s is a short string (<=100).

    A subsequence of a string is a new string which is formed from
    the original string by deleting some (can be none) of the characters
    * without disturbing the relative positions of the remaining characters. *
    (ie, "ace" is a subsequence of "abcde" while "aec" is not).

    Example 1:
    s = "abc", t = "ahbgdc"

    Return true.

    Example 2:
    s = "axc", t = "ahbgdc"

    Return false.

    Follow up:
    If there are lots of incoming S, say S1, S2, ... , Sk where k >= 1B,
    and you want to check one by one to see if T has its subsequence.
    In this scenario, how would you change your code?

    val table = Array.ofDim(26)(List())
    26 个 list存T字符串内各个字母的位置
  */
  def isSubsequence(s: String, t: String): Boolean = {
    //双指针
    val cs = s.toCharArray
    var k = 0
    for (i <- 0 until t.length) {
      if (k < cs.length && cs(k) == t(i))
        k += 1
    }

    k == cs.length
  }

  //455. Assign Cookies
  /*
    Assume you are an awesome parent and want to give your children some cookies.
    But, you should give each child at most one cookie.
    Each child i has a greed factor gi,
    which is the minimum size of a cookie that the child will be content with;
    and each cookie j has a size sj. If sj >= gi, we can assign the cookie j to the child i,
    and the child i will be content. Your goal is to maximize the number of your content children
    and output the maximum number.

    Note:
    You may assume the greed factor is always positive.
    You cannot assign more than one cookie to one child.

  */
  def findContentChildren(g: Array[Int], s: Array[Int]): Int = {
    //排序后，demand从左至右找符合的cookie
    val demand = g.sorted
    val cookies = s.sorted
    var j = 0
    var res = 0
    for (i <- demand.indices) {
      while (j < cookies.length && cookies(j) < demand(i))
        j += 1
      // current cookie can feed
      if (j < cookies.length) {
        res += 1
        j += 1
      }
    }

    res
  }

  //55. Jump Game
  /*
    Given an array of non-negative integers, you are initially positioned at the first index of the array.

    Each element in the array represents your maximum jump length at that position.

    Determine if you are able to reach the last index.
  */
  def canJump(nums: Array[Int]): Boolean = {
    //the farthest pos that current time can reach
    var dist = 0
    // standing at current pos then update dist
    for (i <- nums.indices if i <= dist) {
      dist = Math.max(nums(i) + i, dist)
    }

    dist >= nums.length - 1
  }

  //45. Jump Game II
  /*
    Given an array of non-negative integers, you are initially positioned at the first index of the array.

    Each element in the array represents your maximum jump length at that position.

    Your goal is to reach the last index in the minimum number of jumps.

    val jumps = Array.fill(nums.length)(Integer.MAX_VALUE)
    由于每次只能跳一步，jumps可能的结果，每一段只能从前一段跳一步得到
    0 1 1 1 2 2 2 2 2 3 3 3 4 4


  */
  def jump(nums: Array[Int]): Int = {
    /*
     /* patially passed */
   val jumps = Array.fill(nums.length)(Integer.MAX_VALUE)
   jumps(0) = 0
   val visited = Array.fill(nums.length)(false)
   visited(0) = true
   var queue = scala.collection.immutable.Queue(0)

   //path through
   while (queue.nonEmpty) {
     val curIndex = queue.dequeue._1
     val curJumps = jumps(curIndex)
     if (curIndex == (nums.length - 1))
       return jumps(curIndex)

     visited(curIndex) = true
     queue = queue.dequeue._2

     // possible pos of next jump
     if (nums(curIndex) == 0) //do nothing
     {}
     else (1 to nums(curIndex))
       .toStream
       .map(_ + curIndex)
       .filter(_ < nums.length)
       .foreach(x => {
         if(visited(x))
         {}
         else{
           jumps(x) = Math.min(jumps(x), curJumps + 1)
           queue = queue.enqueue(x)
         }
       })

   }

   0

   */

    if (nums.length == 1) return 0
    var l, r, step = 0
    while (l <= r && r < nums.length) {
      var curCanReach = 0
      for (i <- l to r)
        curCanReach = Math.max(curCanReach, i + nums(i))
      l = r + 1
      r = curCanReach
      step += 1
      if (r >= nums.length - 1)
        return step
    }

    step
  }

  //376. Wiggle Subsequence
  def wiggleMaxLength(nums: Array[Int]): Int = {
    /*
      状态跳变
    */
    if (nums.length <= 1)
      return nums.length
    var res = 1
    var upDownStatu: Int = 0

    for (i <- 1 until nums.length) {
      if (nums(i) > nums(i - 1) && (upDownStatu == 0 || upDownStatu == 1)) {
        upDownStatu = 2
        res += 1
      }
      if (nums(i) < nums(i - 1) && (upDownStatu == 0 || upDownStatu == 2)) {
        upDownStatu = 1
        res += 1
      }
    }

    res
  }

  //406. Queue Reconstruction by Height
  /*
    Suppose you have a random list of people standing in a queue.
    Each person is described by a pair of integers (h, k),
    where h is the height of the person and k is the number of people in front of this person
    who have a height greater than or equal to h. Write an algorithm to reconstruct the queue.

    Example

    Input:
    [[7,0], [4,4], [7,1], [5,0], [6,1], [5,2]]

    Output:
    [[5,0], [7,0], [5,2], [6,1], [4,4], [7,1]]

  */
  def reconstructQueue(people: Array[Array[Int]]): Array[Array[Int]] = {
    /*
      按高度先排序，总是高在前
      然后按顺序重组，因为始终是高的先进list，后进的必定矮，
      k值作下标插入
    */
    val orderByHeight = people.sortWith((a, b) => {
      if (a(0) == b(0))
        a(1) - b(1) < 0
      else
        a(0) - b(0) > 0
    })

    val res = ArrayBuffer[Array[Int]]()
    for (ele <- orderByHeight)
      res.insert(ele(1), ele)

    res.toArray
  }

  //452. Minimum Number of Arrows to Burst Balloons
  /*
   There are a number of spherical balloons spread in two-dimensional space. For each balloon, provided input is the start and end coordinates of the horizontal diameter. Since it's horizontal, y-coordinates don't matter and hence the x-coordinates of start and end of the diameter suffice. Start is always smaller than end. There will be at most 104 balloons.

   An arrow can be shot up exactly vertically from different points along the x-axis. A balloon with xstart and xend bursts by an arrow shot at x if xstart ≤ x ≤ xend. There is no limit to the number of arrows that can be shot. An arrow once shot keeps travelling up infinitely. The problem is to find the minimum number of arrows that must be shot to burst all balloons.

   Input:
    [[10,16], [2,8], [1,6], [7,12]]

    Output:
    2

    Explanation:
    One way is to shoot one arrow for example at x = 6 (bursting the balloons [2,8] and [1,6]) and another arrow at x = 11 (bursting the other two balloons).

    每一个Array代表每一个气球直径的开始和结束坐标
    每次可以从x轴垂直发射一支箭戳破气球，求最少的发射次数

    按结束坐标排序
    默认第一支箭指向第一个元素的结束坐标，
    不断往后找，直到第一支箭的坐标小于某个气球的开始坐标
    第二支箭指向“某个”气球的结束坐标

    循环往复

  */
  def findMinArrowShots(points: Array[Array[Int]]): Int = {
    if (points.isEmpty)
      return 0
    else if (points.length == 1)
      return 1

    val sortedPoints = points.sortWith((a, b) => a(1) < b(1))
    //first arrow point to first balloon's end pos
    var arrowCount = 1
    var arrowPos = sortedPoints(0)(1)
    // if arrowPos greater or equal than current balloon's start pos
    // that means current arrow can take current balloon
    // also means no needs for another arrow
    var i = 1
    while (i < sortedPoints.length) {
      if (arrowPos >= sortedPoints(i)(0))
        i += 1
      //find next arrowPos
      else {
        arrowCount += 1
        arrowPos = sortedPoints(i)(1)
        i += 1
      }
    }

    @tailrec
    def getArrowCount(sortedPoints: Array[Array[Int]], i: Int, arrowPos: Int, arrowCount: Int): Int = {
      if (i >= sortedPoints.length)
        arrowCount
      else {
        if (arrowPos >= sortedPoints(i)(0))
          getArrowCount(sortedPoints, i + 1, arrowPos, arrowCount)
        else
          getArrowCount(sortedPoints, i + 1, sortedPoints(i)(1), arrowCount + 1)
      }
    }

    getArrowCount(sortedPoints, 1, sortedPoints(0)(1), 1)
  }

  //402. Remove K Digits
  /*
    Given a non-negative integer num represented as a string, remove k digits from the number so that the new number is the smallest possible.

    Note:
    The length of num is less than 10002 and will be ≥ k.
    The given num does not contain any leading zero.
    Example 1:

    Input: num = "1432219", k = 3
    Output: "1219"
    Explanation: Remove the three digits 4, 3, and 2 to form the new number 1219 which is the smallest.

    原则上用低位数字小的数换掉高位数字大的数
    栈记录从左至右遍历的上一个数
    若当前出现的比top指向的小，弹出(可能多次)，压入新值

    上述操作完成后，去除前面的0，然后弹出进List

  */
  def removeKdigits(num: String, k: Int): String = {
    if (num.length == 0 || num.length - k == 0)
      return "0"

    val stack = new scala.collection.mutable.Stack[Char]()
    stack.push(num(0))
    var kk = k

    //remove digits
    for (i <- 1 until num.length) {
      val c = num(i)
      while (kk > 0 && stack.nonEmpty && stack.top > c) {
        stack.pop()
        kk -= 1
      }
      stack.push(c)
    }

    //dual with corner case "1111"
    while(kk > 0){
      stack.pop()
      kk -= 1
    }

    //form string
    val str = stack.toArray.reverse.foldLeft("")(_ + _)

    //get rid of the leading '0's
    var idx = 0
    while (idx < str.length && str(idx) == '0')
      idx += 1

    // get res according to logic
    if(idx >= str.length)
      "0"
    else
      str.substring(idx)
  }

  //134. Gas Station
  /*
    There are N gas stations along a circular route, where the amount of gas at station i is gas[i].

    You have a car with an unlimited gas tank and it costs cost[i] of gas to travel from station i to its next station (i+1). You begin the journey with an empty tank at one of the gas stations.

    Return the starting gas station's index if you can travel around the circuit once in the clockwise direction, otherwise return -1.

    Note:

    If there exists a solution, it is guaranteed to be unique.
    Both input arrays are non-empty and have the same length.
    Each element in the input arrays is a non-negative integer.
    Example 1:

    Input:
    gas  = [1,2,3,4,5]
    cost = [3,4,5,1,2]

    Output: 3

    Explanation:
    Start at station 3 (index 3) and fill up with 4 unit of gas. Your tank = 0 + 4 = 4
    Travel to station 4. Your tank = 4 - 1 + 5 = 8
    Travel to station 0. Your tank = 8 - 2 + 1 = 7
    Travel to station 1. Your tank = 7 - 3 + 2 = 6
    Travel to station 2. Your tank = 6 - 4 + 3 = 5
    Travel to station 3. The cost is 5. Your gas is just enough to travel back to station 3.
    Therefore, return 3 as the starting index.

    if sum(gas[i]) - sum(cost[i]) > 0 意味着可以走完一圈

    第一遍判断是否可以走完
    第二遍找开始的位置

    tank += gas[i] - cost[i]
    if(tank < 0){
      start += 1
      tank = 0
    }

  */
  def canCompleteCircuit(gas: Array[Int], cost: Array[Int]): Int = {
//    val total = gas.zip(cost).foldLeft(0)((a,b) => {a + b._1 - b._2})
//    if(total < 0)
//      return -1

    var start = 0
    var tank = 0
    var sumGas = 0
    var sumCost = 0
    for(i <- gas.indices){
      sumGas += gas(i)
      sumCost += cost(i)
      tank = tank + gas(i) - cost(i)
      // current pos couldn't move on
      // init next pos be the start pos
      if(tank < 0){
        start = i + 1
        tank = 0
      }
    }

    if(sumGas < sumCost)
      -1
    else
      start
  }


}
