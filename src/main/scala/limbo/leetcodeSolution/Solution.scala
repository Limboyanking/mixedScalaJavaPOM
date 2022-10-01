package limbo.leetcodeSolution

import scala.annotation.tailrec
import scala.collection.mutable

object Solution {

  /*
    关于时间复杂度
    一般ACM或者笔试题的时间限制是1秒或2秒。
    在这种情况下，C++代码中的操作次数控制在 10^7 为最佳。

    下面给出在不同数据范围下，代码的时间复杂度和算法该如何选择：

    n≤30, 指数级别, dfs+剪枝，状态压缩dp

    n≤100 => O(n^3)，floyd，dp

    n≤1000 => O(n^2)，O(n^2 * log(n))，dp，二分

    n≤10000 => O(n ∗ sqrt(n))，块状链表

    n≤100000 => O(n * log(n))
      => 各种sort，线段树、树状数组、
      set/map、heap、dijkstra+heap、spfa、求凸包、求半平面交、二分

    n≤1000000 => O(n) 以及常数较小的 O(n * log(n))
     算法 => hash、双指针扫描、kmp、AC自动机，常数比较小的
     O(n * log(n)) 的做法：sort、树状数组、heap、dijkstra、spfa

    n≤10000000 => O(n)
    双指针扫描、kmp、AC自动机、线性筛素数

    n≤10^9 => O(sqrt(n))，判断质数

    n≤10^18 => O(log(n))，最大公约数

    n = 100000:

    - O(n) = 100000
    - O(n^2) = 10^10
    - O(n * log(n)) = 28n = 2 * 10^5

  */

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }

  //2 输入两个链表，返回一个新链表，其中各节点是两个链表各节点的和(每次各节点累加带进位)
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    var p = l1
    var q = l2
    val dummy = new ListNode()
    var current = dummy
    dummy.next = current
    var carry = 0
    //at least one link nonEmpty
    while(p != null || q != null){
      var sum = (p,q) match {
        case (null,_) => carry + q.x
        case (_,null) => carry + p.x
        case _ => carry + p.x + q.x
      }
      carry = sum / 10
      current.next = new ListNode(sum % 10)
      current = current.next
      if(p != null) p = p.next
      if(q != null) q = q.next
    }

    //in case of carry still greater then 0
    while(carry > 0){
      current.next = new ListNode(carry)
      current = current.next
      carry /= 10
    }

    dummy.next
  }

  //3 在一个字符串中找到字母不重复的最长子串，把这个子串的长度返回
  def lengthOfLongestSubstring(s: String): Int = {
    var checkMap = Map[Char, Int]()
    var start, len = 0

    for (i <- 0 to s.length - 1) {
      val cur = s.charAt(i)
      if (checkMap.contains(cur)) {
        //当前字符下标大于等于子字符串开始下标
        if (checkMap(cur) >= start)
          start = checkMap(cur) + 1
      }
      len = Math.max(len, i - start + 1)
      checkMap += (cur -> i)
    }

    len
  }

  //5 找到字符串中的最长回文子串 Manacher Algorithm 1975 O(n)
  def longestPalindrome(s: String): String = {
    /**
      * 给字符串加分割符来将奇偶回文都按奇回文处理。
      * 并通过在遍历奇回文中的以每个字符为中心的回文子串时，
      * 利用i < mx时可预测以i为中心的回文半径的最小大小来减少回文计算次数
      * *
      * "$1#2#3#2#1#"
      * p[i]表示以t[i]字符为中心的回文子串的半径，若p[i] = 1，则该回文子串就是t[i]本身
      * 最长子串的长度是半径减1，起始位置是中间位置减去半径再除以2
      *
      */
    val head = "$#"
    val procString = head + s.flatMap(x => x + "#")
    var p = Array.fill(procString.length)(0)
    /*

    mx的对称点       j              id             i             mx

    ---|-------------|--------------|--------------|--------------|-----

    */
    /**
      * id为能延伸到最右端的位置的那个回文子串的中心点位置
      * mx是回文串能延伸到的最右端的位置
      * *
      * 如果当前下标仍然在最右边的子回文内：
      * 如果p(j)没有跑出子回文，因为对称关系，p(i) = p(j)，往右继续找
      * 如果p(j)跑出子回文，p(i)至少有 mx - i 的大小, p(i) = mx - i
      * *
      * 如果当前下标超出最右的子回文，只能一个一个往前找回文：
      * p(i) = 1
      *
      */

    var mx, id, resRadius, resCenter = 0
    for (i <- 1 until procString.length) {
      val j = 2 * id - i
      p(i) = if (mx > i) Math.min(p(j), mx - i) else 1

      /** $  #  b  #  a  #  b  #  a  #  d   #
        * 0  1  2  3  4  5  6  7  8  9  10  11
        */

      //当前i位置新的回文半径有效
      while (
        (if ((i + p(i)) == procString.length) "!" else procString(i + p(i)))
          ==
          procString(i - p(i))) {
        p.update(i, p(i) + 1)
      }
      //如果i位置的回文半径超出当前最右回文，移动范围
      if (mx < i + p(i)) {
        mx = i + p(i)
        id = i
      }
      if (resRadius < p(i)) {
        resRadius = p(i)
        resCenter = i
      }
    }

    val start = (resCenter - resRadius) / 2
    val end = start + resRadius - 1

    val res = s.substring(start, end)

    res
  }

  //6 将字符串按给定行数且按照N的形状重排
  def convert(s: String, numRows: Int): String = {
    if (numRows == 1)
      return s

    val rows = Array.fill(Math.min(numRows, s.length))("")
    var curRow = 0
    var goingDown = false

    //碰到一个字符放到一层，触底或者碰高就转换方向
    for (c <- s) {
      rows(curRow) = rows(curRow) + c
      if (curRow == 0 || curRow == numRows - 1)
        goingDown = !goingDown
      if (goingDown)
        curRow = curRow + 1
      else
        curRow = curRow - 1
    }

    val res = rows.foldLeft("")((a, b) => a + b)

    res
  }

  //7 整型数倒序且不可越界
  def reverse(x: Int): Int = {
    var res = 0
    var xx = x
    while (xx != 0) {
      var pop = xx % 10
      xx = xx / 10
      if (res > Integer.MAX_VALUE / 10 ||
        (res == Integer.MAX_VALUE / 10 && pop > 7))
        return 0
      if (res < Integer.MIN_VALUE / 10 ||
        (res == Integer.MIN_VALUE / 10 && pop < -8))
        return 0

      res = res * 10 + pop
    }
    res
  }

  //8 读取字符串中的数字，转换为整型
  def myAtoi(str: String): Int = {
    var sign = 1
    var base, i = 0
    while (str(i).toString == " ")
      i = i + 1

    while (str(i).toString == "-" || str(i).toString == "+") {
      val next = i + 1
      if (str(next).toString == "+" || str(next).toString == "-")
        sign = 0
      if (sign != 0 && str(i).toString == "-")
        sign = -1

      i = i + 1
    }

    base = str(i).getNumericValue

    while (str(i).getNumericValue >= 0 && str(i).getNumericValue <= 9 && i < str.length - 1) {
      if (base > Integer.MAX_VALUE / 10 ||
        (base == Integer.MAX_VALUE / 10 && str(i).getNumericValue - 0 > 7)) {
        if (sign == 1) return Integer.MAX_VALUE
        else Integer.MIN_VALUE
      }
      val next = i + 1
      base = 10 * base + str(next).getNumericValue
      i = next
    }

    base * sign
  }

  //9 判断一个整型数是否回文
  def isPalindrome(x: Int): Boolean = {
    /*
      从尾部开始一位一位要，直到比前半部分大为止，
      判断是否与前半部分一致
    */
    var _x = x
    if (_x < 0 || (_x % 10 == 0 && _x != 0))
      return false

    var revertedNum = 0
    while (_x > revertedNum) {
      revertedNum = revertedNum * 10 + _x % 10
      _x = _x / 10
    }

    val ret =
      if (_x == revertedNum || _x == revertedNum / 10)
        true
      else false

    ret
  }

  //10 模拟正则表达式 . * 功能
  def isMatch(s: String, p: String): Boolean = {


    false
  }

  //12 整型转换至罗马数字
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

  //13 罗马数字转整型数 433ms
  def romanToInt(s: String): Int = {
    // 不存在类似IIV的情况
    // 原串 MCMXCIV
    // foldLeft c1 < c2 => 求值(c2 - 2 * c1)
    //          else c1 >= c2 => c2
    val relation = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)

    //                                累计值，上一个char
    val res = s.toCharArray.foldLeft((0, 0))((a, b) => {
      val ret =
        if (a._1 == 0)
          (relation(b), relation(b))
        else if (a._2 >= relation(b))
          (a._1 + relation(b), relation(b))
        else
          (a._1 + relation(b) - 2 * a._2, relation(b))

      ret
    })

    res._1
  }

  //14 多个字符串中找最长的公共部分
  def longestCommonPrefix(strs: Array[String]): String = {
    //Instructional Programming IP
    //    if (strs.length == 0)
    //      return ""
    //    var prefix = strs(0)
    //    var i = 1
    //    for(i <- 1 until strs.length){
    //      //当前字符串不以 prefix开头
    //      while(strs(i).indexOf(prefix) != 0){
    //        prefix = prefix.substring(0,prefix.length - 1)
    //        if(prefix.isEmpty)
    //          return ""
    //      }
    //    }
    //
    //    prefix


    @tailrec
    def commonPrefix(str: String, prefix: String): String = {
      if (prefix.isEmpty) ""
      else if (str.startsWith(prefix)) prefix
      else commonPrefix(str, prefix.substring(0, prefix.length - 1))
    }

    if (strs.isEmpty) ""
    else strs.foldLeft(strs.head)((prefix, str) => commonPrefix(str, prefix))

  }

  //15 找List 内相加为0 的三个数，结果不可出现重复的三元元素
  def threeSum(nums: Array[Int]): List[List[Int]] = {
    def chkDup(lo: Int, hi: Int, arr: Array[Int]): (Int, Int) = {
      if (lo < hi && arr(lo) == arr(lo + 1))
        chkDup(lo + 1, hi, arr)
      else if (lo < hi && arr(hi) == arr(hi - 1))
        chkDup(lo, hi - 1, arr)
      else
        (lo, hi)
    }

    // -4 -1 -1 0 1 2
    def persue(sum: Int, lo: Int, hi: Int, arr: Array[Int]): (Int, Int) = {
      if (lo < hi && (sum == arr(lo) + arr(hi))) {
        (lo, hi)
      } else if (lo < hi && (sum > arr(lo) + arr(hi)))
        persue(sum, lo + 1, hi, arr)
      else if (lo < hi && (sum < arr(lo) + arr(hi)))
        persue(sum, lo, hi - 1, arr)
      else
        (lo, hi)
    }

    def getCurRes(sum: Int, sorted: Array[Int], lo: Int, hi: Int, curRes: List[List[Int]]) = {
      var _curRes = curRes
      var _lo = lo
      var _hi = hi
      while (_lo < _hi) {
        val p = persue(sum, _lo, _hi, sorted) //找到当前符合条件的位置

        if (p._1 < p._2 && sum == sorted(p._1) + sorted(p._2)) //过滤碰到一块以及不符合的情况
          _curRes = _curRes :+ List(-sum, sorted(p._1), sorted(p._2)) //存入

        val pp = chkDup(p._1, p._2, sorted) //找到当前符合条件且最靠中心的位置

        _lo = pp._1 + 1 //继续往内找
        _hi = pp._2 - 1
      }

      (_curRes, (lo + 1, hi), Option(-sum))
    }

    if (nums.length < 3)
      return List[List[Int]]()

    val sorted = nums.sorted
    val area = (0, sorted.length - 1)
    //只跟前一个比较，相同值只取一次, lo 到 hi的范围可能有多组，不断往内收缩找新的组合
    // ret: List( List(List[Int]()) )
    // -4 -1 -1 0 1 2           (结果,以b开头的List对应的tial,上一个)
    val ret = sorted.foldLeft((List[List[Int]](), area, None: Option[Int]))((a, b) => {
      val r =
      //第一个
        if (a._3.isEmpty)
          getCurRes(-b, sorted, a._2._1 + 1, a._2._2, a._1)
        else if (a._3.get != b)
          getCurRes(-b, sorted, a._2._1, a._2._2, a._1)
        else
          (a._1, (a._2._1 + 1, a._2._2), a._3)

      r
    })._1

    ret

    /*
      // 排序后比较 -arr(i) == arr(lo) + arr(hi)
// lo hi 均往中间靠拢，试图找可以满足条件的位置
var res = List[List[Int]]()
val arr = nums.sorted

for(i <- arr.indices){
  var lo = i + 1
  var hi = arr.length - 1
  val sum = -arr(i)
  //只跟前一个比较，相同值只取一次, lo 到 hi的范围可能有多组，不断往内收缩找新的组合
  if(i == 0 || i > 0 && arr(i - 1) != arr(i)){
    while(lo < hi){
      if(sum == arr(lo) + arr(hi)){
        res = res.+:(List(arr(i),arr(lo),arr(hi)))
        while(lo < hi && arr(lo) == arr(lo + 1))
          lo += 1
        while(lo < hi && arr(hi) == arr(hi - 1))
          hi -= 1

        lo += 1
        hi -= 1
      }else if(sum < arr(lo) + arr(hi)) //最右位置较大值往中间小值靠拢
        hi -= 1
      else lo += 1
    }
  }
}

res
  */
  }

  //16 找List 内的相加最接近target 的三个元素之和
  def threeSumClosest(nums: Array[Int], target: Int): Int = {

    //在参数内更新result
    def getValue(result: Int, cur: Int, target: Int, sorted: Array[Int], lo: Int, hi: Int): Int = {
      var curSum = 0
      if (lo < hi)
        curSum = cur + sorted(lo) + sorted(hi)

      if (lo >= hi) //范围内遍历结束
        result
      else if (curSum == target) //相等直接返回
        curSum
      else if (Math.abs(curSum - target) < Math.abs(result - target)) {
        //当前满足条件，存入，继续找
        if (target > curSum)
          getValue(curSum, cur, target, sorted, lo + 1, hi)
        else
          getValue(curSum, cur, target, sorted, lo, hi - 1)
      }
      else if (target > curSum)
        getValue(result, cur, target, sorted, lo + 1, hi)
      else if (target < curSum)
        getValue(result, cur, target, sorted, lo, hi - 1)
      else
        result

    }

    def getCurRes(cur: Int, target: Int, result: Int, sorted: Array[Int], lo: Int, hi: Int) = {
      val v = getValue(result, cur, target, sorted, lo, hi)

      (Some(v), (lo + 1, hi))
    }

    if (nums.length < 3)
      return 0

    val result = nums(0) + nums(1) + nums(2)
    val sorted = nums.sorted
    val area = (0, sorted.length - 1)

    //                              结果       前一个
    val ret = sorted.foldLeft((None: Option[Int], 0, area))((a, b) => {
      val ret =
        if (a._1.isEmpty) {
          val ret = getCurRes(b, target, result, sorted, area._1 + 1, area._2)
          (ret._1, b, ret._2)
        }
        else if (b == a._2 || a._3._1 == a._3._2) // 当前元素与前一个一致或者最后只剩两个，不算
          (a._1, a._2, (a._3._1 + 1, a._3._2))
        else {
          val ret = getCurRes(b, target, a._1.get, sorted, a._3._1, a._3._2)
          (ret._1, b, ret._2)
        }

      ret
    })

    ret._1.get

    //  if(nums.length < 3)
    //    return 0
    //
    //  var result: Int = nums(0) + nums(1) + nums(2)
    //  val sorted = nums.sorted
    //
    //  for(i <- 0 until sorted.length - 2){
    //    if(i > 0 && sorted(i) == sorted(i-1)){
    //
    //    }else{
    //      var start = i + 1
    //      var end = sorted.length - 1
    //      var sum = 0
    //      while(start < end){
    //        sum = sorted(i) + sorted(start) + sorted(end)
    //        if(sum == target)
    //          return sum
    //        if(Math.abs(sum - target) < Math.abs(result - target))
    //          result = sum
    //
    //        if(sum > target)
    //          end -= 1
    //        else
    //          start += 1
    //      }
    //    }
    //
    //  }
    //
    //  result

  }

  //17 电话键盘数字组合转换为子母组合
  def letterCombinations(digits: String): List[String] = {
    //第几位数字对应当前弹出的组合有多少个子母，
    //弹上一此处理完的组合结果与当前数字子母组合结合，处理完压栈
    /*
      val ans = new java.util.LinkedList[String]()
      ans.add("")

      var buffer = List[String]()

      if(digits.isEmpty)
        return List[String]()

      val mapping = Array("0", "1", "abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz")
      for(i<-0 until digits.length){
        val x = digits.charAt(i).getNumericValue //当前数字

        //处理完的子母组合长度恰好与数字的下标一致
        while(ans.peek().length == i){
          val t = ans.remove() //拿上一次已处理的一个组合
          for(c <- mapping(x).toCharArray)
            ans.add(t + c) // 与需要处理的字符结合
        }
      }

      while(ans.peek != null)
        buffer = buffer :+ ans.peek()

      buffer
    */

    if (digits == "")
      return List[String]()

    val mapping = Array("0", "1", "abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz")
    val ans = digits.toCharArray.foldLeft(List[String]())((a, b) => {
      if (a.isEmpty)
        mapping(b.getNumericValue).toCharArray.map(_.toString).toList
      else {
        val res = a.flatMap(s => {
          val n = mapping(b.getNumericValue).toCharArray.map(c => s + c)
          n
        })

        res
      }
    })

    ans
  }

  //19 去除从List后边开始的第N个元素
  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    /**
      * dummy 接头
      * second first 初始在 dummy
      * first 往前走N 次
      * second first 同步走，直到first 走到最后一个
      *second.next = second.next.next
      * *
      * 返回 dummy.next
      */
    def getN(_second: ListNode, _first: ListNode): (ListNode, ListNode) = {
      if (_first.next == null)
        (_second, _first)
      else if (_first.next != null)
        getN(_second.next, _first.next)
      else
        null
    }

    def getGap(node: ListNode, n: Int): ListNode = {
      if (n == 0)
        node
      else if (n > 0)
        getGap(node.next, n - 1)
      else
        null
    }

    val dummy = new ListNode(0)
    dummy.next = head
    val first = dummy
    val second = dummy

    val first1 = getGap(first, n)
    val preN = getN(second, first1)._1
    preN.next = preN.next.next

    dummy.next
  }

  //20 判断一串以 { < ( ] 组成的字符串是否闭合
  def isValid(s: String): Boolean = {
    val stack = new java.util.Stack[Char]()
    for (i <- 0 until s.length) {
      // Push any open parentheses onto stack
      if (s.charAt(i) == '(' || s.charAt(i) == '[' || s.charAt(i) == '{')
        stack.push(s.charAt(i))
      // Check stack for corresponding closing parentheses, false if not valid
      else if (s.charAt(i) == ')' && !stack.empty() && stack.peek() == '(')
        stack.pop()
      else if (s.charAt(i) == ']' && !stack.empty() && stack.peek() == '[')
        stack.pop()
      else if (s.charAt(i) == '}' && !stack.empty() && stack.peek() == '{')
        stack.pop()
      else
        return false
    }

    false
  }

  //21 合并两串有序的List
  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
    if (l1 == null) return l2
    if (l2 == null) return l1

    def process(dummy: ListNode, pre: ListNode, l1: ListNode, l2: ListNode): ListNode = {
      if (l1 == null) {
        pre.next = l2
        dummy.next
      }
      else if (l2 == null) {
        pre.next = l1
        dummy.next
      }
      else if (l1.x < l2.x) {
        pre.next = l1
        process(dummy, l1, l1.next, l2)
      }
      else {
        pre.next = l2
        process(dummy, l2, l1, l2.next)
      }
    }

    val dummy = new ListNode(0)
    val ans = process(dummy, dummy, l1, l2)

    ans
  }

  //22  给定N对 ( ) , 找出所有有效的组合
  def generateParenthesis(n: Int): List[String] = {
    /*
     f(0): ""

     f(1): "("f(0)")"

     f(2): "("f(0)")"f(1), "("f(1)")"

     f(3): "("f(0)")"f(2), "("f(1)")"f(1), "("f(2)")"

     So f(n) = "("f(0)")"f(n-1) , "("f(1)")"f(n-2) "("f(2)")"f(n-3) ... "("f(i)")"f(n-1-i) ... "(f(n-1)")"

     i <- 0 to n - 1
     ( f(i) ) f(n - i -1)
     i + 1 + n - i - 1 = n 对括号

     ∑( f(i) ) f(n - i -1)  +  ( f(n - 1) )

     */

    def getCurFn(fnList: List[List[String]], curFn: List[List[String]], i: Int, cur: Int): List[List[String]] = {
      if (i > cur - 1)
        curFn
      else {
        val rightPos = cur - i - 1
        val fi = fnList(i)
        val fr = fnList(rightPos)
        val curRes =
          if (i == cur - 1)
            fi.map(s => "(" + s + ")")
          else if (i == 0)
            fr.map(s => "()" + s)
          else
            fi.flatMap(i => {
              fr.map(r => "(" + i + ")" + r)
            })

        getCurFn(fnList, curFn :+ curRes, i + 1, cur)
      }
    }

    def getFn(fnList: List[List[String]], cur: Int, n: Int): List[String] = {
      if (cur > n)
        fnList(n)
      else if (cur <= n) {
        val curFn = getCurFn(fnList, List(List()), 0, cur).flatten
        getFn(fnList :+ curFn, cur + 1, n)
      }
      else
        List()
    }

    getFn(List(List("")), 1, n)
  }

  //23 将k条有序的ListNode链表，整合成一条
  def mergeKLists(lists: Array[ListNode]): ListNode = {
    /*
      不得已修改原lists

      interval

      i <- 0 to res.length - interval
      保证所有元素两两归约

      interval * 2
    */

    //    def merge(lists: Array[ListNode], res: ListNode, i: Int, interval: Int): ListNode = {
    //      if (i > lists.length - interval)
    //        res
    //      else {
    //        val cur =
    //          if(i + interval > lists.length)
    //            lists(i)
    //          else
    //            mergeTwoLists(lists(i), lists(i + interval))
    //        lists(i) = cur
    //
    //        merge(lists, lists(0), i + interval * 2, interval)
    //      }
    //    }
    //
    //    def intervalMerge(lists: Array[ListNode], interval: Int): ListNode = {
    //      if (interval > lists.length)
    //        lists(0)
    //      else {
    //        merge(lists, lists(0), 0, interval)
    //        intervalMerge(lists, interval * 2)
    //      }
    //    }
    //
    //    intervalMerge(lists,1)


    def devide(lists: Array[ListNode], s: Int, e: Int): ListNode = {
      if (s == e)
        lists(s)
      if (s < e) {
        val m = (s + e) / 2
        val l1 = devide(lists, s, m)
        val l2 = devide(lists, m + 1, e)

        mergeTwoLists(l1, l2)
      } else
        null
    }

    devide(lists, 0, lists.length - 1)
  }

  //24 将链表中每两个相邻的节点交换位置
  def swapPairs(head: ListNode): ListNode = {

    def reverse(res: ListNode = null, head: ListNode, pre: ListNode, a: ListNode, b: ListNode): ListNode = {
      if (a.next == null)
        res
      else if (pre == null) {
        //第一次调转
        a.next = b.next
        b.next = a

        if (a.next == null)
          b
        else
          reverse(b, head, a, a.next, a.next.next)
      } else if (a != null && b != null) {
        a.next = b.next
        b.next = a
        pre.next = b

        if (a.next == null)
          res
        else
          reverse(res, head, a, a.next, a.next.next)
      }
      else
        new ListNode()
    }

    if (head == null || head.next == null)
      head
    else
      reverse(null, head, null, head, head.next)
  }

  //54 Spiral Matrix
  /*
    Given a matrix of m x n elements (m rows, n columns),
    return all elements of the matrix in spiral order
  */
  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
    var listBuffer = scala.collection.mutable.ListBuffer[Int]()
    if (matrix == null || matrix.length == 0)
      return List()

    var rows = matrix.length
    var cols = matrix(0).length
    var ceiling = 0
    var floor = rows - 1
    var leftMost = 0
    var rightMost = cols - 1

    while (listBuffer.length < rows * cols) {
      //be care of turning point
      //go right
      for (j <- leftMost to rightMost if listBuffer.length < rows * cols)
        listBuffer += matrix(ceiling)(j)
      //go down
      for (i <- ceiling + 1 to floor if listBuffer.length < rows * cols)
        listBuffer += matrix(i)(rightMost)
      //go left
      for (j <- rightMost - 1 to leftMost by -1 if listBuffer.length < rows * cols)
        listBuffer += matrix(floor)(j)
      //go up
      for (i <- floor - 1 to ceiling + 1 by -1 if listBuffer.length < rows * cols)
        listBuffer += matrix(i)(leftMost)

      //shrinking the boundary square
      ceiling += 1
      floor -= 1
      leftMost += 1
      rightMost -= 1
    }

    listBuffer.toList
  }

  //121 Best Time to Buy and Sell Stock Array中第i个元素代表第i天股票价格 只交易一次，买入、卖出，求最大获利 必须先买入后卖出
  def maxProfit(prices: Array[Int]): Int = {
    if (prices.length < 2)
      return 0

    var minPri = prices(0)
    var maxPro = 0
    for (i <- 1 until prices.length) {
      if (prices(i - 1) < prices(i)) {
        if (prices(i) - minPri > maxPro)
          maxPro = prices(i) - minPri
      } else if (prices(i) < minPri)
        minPri = prices(i)
    }

    maxPro
  }

  //122 easy 可交易多次的股票，但买入前必须先卖出，求最大利润
  def maxProfit2(prices: Array[Int]): Int = {
    if (prices.length < 2)
      return 0

    def transaction(i: Int, buyPos: Int, sellPos: Int, profit: Int, prices: Array[Int]): Int = {
      if (buyPos + 1 >= prices.length || sellPos + 1 >= prices.length || i + 1 >= prices.length)
        return profit + prices(sellPos) - prices(buyPos)
      if (prices(i) >= prices(i + 1)) //往后找买入的点
        transaction(i + 1, buyPos + 1, sellPos + 1, profit, prices)
      else if (prices(sellPos) <= prices(sellPos + 1)) //找当前最大售价的位置
        transaction(i, buyPos, sellPos + 1, profit, prices)
      else if (prices(sellPos) > prices(sellPos + 1)) { //可售价减小的位置
        val profi = prices(sellPos) - prices(buyPos)
        transaction(sellPos + 1, sellPos + 1, sellPos + 1, profit + profi, prices)
      }
      else
        0
    }

    transaction(0, 0, 0, 0, prices)
  }

  //123 hard 只能最多交易两次的股票，但买入前必须先卖出，求最大利润
  def maxProfit3(prices: Array[Int]): Int = {
    /**
      * *
      * i means the best choice for the strategy at pos i
      * ether choice i pos or i - 1 pos
      * Optimize
      * *
      * sell2(i)
      */


    var buy1, buy2 = Integer.MIN_VALUE
    var sell1, sell2 = 0

    for (i <- prices.indices) {
      buy1 = Math.max(-prices(i), buy1)
      sell1 = Math.max(buy1 + prices(i), sell1)
      buy2 = Math.max(sell1 - prices(i), buy2)
      sell2 = Math.max(buy2 + prices(i), sell2)
    }

    sell2
  }

  //188 hard 至多交易k次，买入前必须先卖出，求最大利润
  def maxProfit4(k: Int, prices: Array[Int]): Int = {
    if (2 * k > prices.length) // k次交易可以覆盖所有交易日
      return maxProfit2(prices)

    def maxProfit2(prices: Array[Int]): Int = {
      if (prices.length < 2)
        return 0

      @tailrec
      def transaction(i: Int, buyPos: Int, sellPos: Int, profit: Int, prices: Array[Int]): Int = {
        if (buyPos + 1 >= prices.length || sellPos + 1 >= prices.length || i + 1 >= prices.length)
          return profit + prices(sellPos) - prices(buyPos)
        if (prices(i) >= prices(i + 1)) //往后找买入的点
          transaction(i + 1, buyPos + 1, sellPos + 1, profit, prices)
        else if (prices(sellPos) <= prices(sellPos + 1)) //找当前最大售价的位置
          transaction(i, buyPos, sellPos + 1, profit, prices)
        else if (prices(sellPos) > prices(sellPos + 1)) { //可售价减小的位置
          val profi = prices(sellPos) - prices(buyPos)
          transaction(sellPos + 1, sellPos + 1, sellPos + 1, profit + profi, prices)
        }
        else
          0
      }

      transaction(0, 0, 0, 0, prices)
    }

    /**
      * opt(i)(j)     在第j日前的第i次交易后可以获得的最大利润
      * *
      * opt(i)(j)
      * opt(i)(j - 1)    or      prices(j) + opt(i - 1)(jj - 1) - prices(jj)  0 < jj < j
      * jj交易日只能卖出 opt(i - 1)(jj - 1) =>  opt(i - 1)(jj)
      * prices(i) + opt(i - 1)(jj) - prices(jj)
      * prices(j) + Max(opt(i - 1)(jj) - prices(jj))
      * Max(opt(i - 1)(jj) - prices(jj)) 理解为完成最后一次卖出前做出的最优选择
      * opt(0)(j)  0
      * opt(i)(0)  0
      *
      */

    val opt = Array.ofDim[Int](k + 1, prices.length)

    for (i <- 1 to k) {
      var tmpBeforeLastBuy = -prices(0)
      for (j <- 1 until prices.length) {
        tmpBeforeLastBuy = math.max(opt(i - 1)(j) - prices(j), tmpBeforeLastBuy)
        opt(i)(j) = math.max(opt(i)(j - 1), prices(j) + tmpBeforeLastBuy)
      }
    }

    opt(k)(prices.length - 1)
  }

  //309 medium 可交易多次，买入前必须卖出，但卖出后需要冷却一天
  def maxProfit5(prices: Array[Int]): Int = {
    /**
      * *
      * i 位置表当前策略可以获得的最大利润
      * *
      * buy(i)
      * cool(i - 1) - prices(i)           buy(i - 1)
      * *
      * sell(i)
      * buy(i - 1) + prices(i)            sell(i - 1)
      * *
      * cool(i)
      * sell(i - 1)        cool(i - 1)
      *
      */

    if (prices.length <= 1)
      return 0

    val buy = Array.fill(prices.length)(0)
    val sell = Array.fill(prices.length)(0)
    val cool = Array.fill(prices.length)(0)

    buy(0) = -prices(0) //在第一交易日只能选择买入
    sell(0) = Integer.MIN_VALUE
    cool(0) = 0

    for (i <- 1 until prices.length) {
      buy(i) = Math.max(cool(i - 1) - prices(i), buy(i - 1))
      sell(i) = Math.max(buy(i - 1) + prices(i), sell(i - 1))
      cool(i) = Math.max(sell(i - 1), cool(i - 1))
    }

    Math.max(cool(prices.length - 1), sell(prices.length - 1))
  }

  //714 medium 可交易多次的股票，买入前必须卖出，每次交易需要支付交易费
  def maxProfit(prices: Array[Int], fee: Int): Int = {
    val buy = Array.fill(prices.length)(0)
    val sell = Array.fill(prices.length)(0)

    if (prices.length <= 1)
      return 0

    buy(0) = -prices(0)
    sell(0) = 0

    for (i <- 1 until prices.length) {
      buy(i) = Math.max(sell(i - 1) - prices(i), buy(i - 1))
      sell(i) = Math.max(buy(i - 1) + prices(i) - fee, sell(i - 1))
    }

    Math.max(buy(prices.length - 1), sell(prices.length - 1))
  }

  //901 medium 求当前交易日之前有多少交易日是股票价格小于等于当前交易日价格的天数(算上当前交易日)
  class StockSpanner() {

    /**
      * Your StockSpanner object will be instantiated and called as such:
      * var obj = new StockSpanner()
      * var param_1 = obj.next(price)
      */

    val s = new java.util.Stack[(Int, Int)]()

    def next(price: Int): Int = {
      var ret = 1 //算上当前交易日
      while (!s.isEmpty && s.peek()._1 <= price) {
        ret = ret + s.peek()._2 //加上当前满足条件的交易日的结果
        s.pop() //去除比当前价格小的数据
      }
      s.push((price, ret)) //压入当前交易日结果
      ret
    }

  }

  //496 num1 是 num2的子集，针对每一个num1内的元素，在nums2内找到第一个数值和下标都比此元素大的元素
  def nextGreaterElement(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    val recordMap = Map[Int, Int]()
    val stack = new java.util.Stack[Int]()

    for (n <- nums2) { //在nums2内部找
      while (!stack.isEmpty && stack.peek < n) //栈顶元素找到第一个下标、数值均比自身大的元素
        recordMap ++ Map(stack.pop -> n) //记录
      stack.push(n)
    }

    val ret = nums1.map(x => recordMap.getOrElse(x, -1))

    ret
  }

  //118 杨辉三角 easy
  def generate(numRows: Int): List[List[Int]] = {

    def build(pre: Stream[Int]): Stream[Int] = pre match {
      case 1 #:: Stream.Empty => Stream(1)
      case x #:: y #:: rest => (x + y) #:: build(y #:: rest)
    }

    @tailrec
    def get(pre: Stream[Int], count: Int, numRows: Int, res: List[List[Int]]): List[List[Int]] = {
      if (numRows == 0)
        List()
      else if (count == 1) {
        val curRes = Stream(1)
        get(curRes, count + 1, numRows, res :+ curRes.toList)
      }
      else if (count <= numRows) {
        //        val tmp1 = Stream(0) ++ pre
        //        val tmp2 = pre ++ Stream(0)
        //
        //        val curRes = tmp1.zip(tmp2).map(x => x._1 + x._2)

        val curRes = Stream.cons(1, build(pre))
        get(curRes, count + 1, numRows, res :+ curRes.toList)
      }
      else
        res
    }

    get(Stream(), 1, numRows, List())
  }

  //662 二叉树的最大宽度 算上空节点 medium
  def widthOfBinaryTree(root: TreeNode): Int = {
    /**
      * Regardless whether these nodes exist:
      * *
      * Always make the id of left child as parent_id * 2;
      * Always make the id of right child as parent_id * 2 + 1;
      * *
      * 外围存一个global的最大宽度变量，在每层递归的时候，
      * 用将最右边的数减去最左边的，然后和外围的global比对即可
      *
      */

    //    def dfs(n: TreeNode, idx: Int, d: Int, lefts: util.ArrayList[Int]): Int = { // d : depth
    //      if (n == null)
    //        return 0
    //      if (d >= lefts.size)
    //        lefts.add(idx) // add left most node
    //      Math.max(idx + 1 - lefts(d), Math.max(dfs(n.left, idx * 2, d + 1, lefts), dfs(n.right, idx * 2 + 1, d + 1, lefts)))
    //    }
    //
    //    val arrayList = new util.ArrayList[Int]()
    //
    //    dfs(root,1,0,arrayList)
    0
  }

  //1122 arr1 包含 arr2，按arr2元素出现的相对位置排序，arr2以外的元素按升序排在后面
  def relativeSortArray(arr1: Array[Int], arr2: Array[Int]): Array[Int] = {

    val ord = arr2.zipWithIndex.toMap.withDefaultValue(Int.MaxValue)
    /*
      // sort by the 3rd element, then 1st
      Sorting.quickSort(pairs)(Ordering[(Int, String)].on(x => (x._3, x._1)))

      def sortBy[B](f: A => B)(implicit ord: Ordering[B]): Repr = sorted(ord on f)
    */

    val ret = arr1.sortBy(x => (ord(x), x))

    ret
  }

  //1155 给定数量骰子、给定每个骰子的可能性，输出所有组合成target的组合数 familiar to 518
  def numRollsToTarget(d: Int, f: Int, target: Int): Int = {
    /**
      * Bottom-Up DP
      * *
      * We can define our dp[i][k] as number of ways we can get k using i dices. As an initial point,
      * there is one way to get to 0 using zero dices.
      * *
      * Then, for each dice i and face j, accumulate the number of ways we can get to k.
      * *
      * Note that for the bottom-up solution, we can reduce our memory complexity as
      * we only need to store counts for the previous dice.
      * *
      * int numRollsToTarget(int d, int f, int target) {
      * vector<int> dp(target + 1);
      * dp[0] = 1;
      * for (int i = 1; i <= d; ++i) {
      * vector<int> dp1(target + 1);
      * for (int j = 1; j <= f; ++j)
      * for (auto k = j; k <= target; ++k)
      * dp1[k] = (dp1[k] + dp[k - j]) % 1000000007;
      * swap(dp, dp1);
      * }
      * return dp[target];
      * }
      * *
      * It's very similar to coin change where you figure out the number of ways to reach a sum given a number of coins.
      * Now in this example we have coins(1,2,3..f) and there are 'd' dices hence the coins available with us are (1,1,1...d times)
      * (2,2,2...d times) ...(f,f,f...d times) .
      * Now it's the standard coin change where you have these coins and you want to figure out number of ways to form target.
      * For that it's the standard solution with dp. You consider all targets from 1 to desired target
      * and start incrementing the value(no of ways to reach target) for each target
      * if that target is reachable by the coins going one by one. That's what the inner most loop represents.
      * Consider the two outer loops as nothing but collecting all the possible coins.
      *
      */


    0
  }

  //1046
  /*
    last Stone Weight
    We have a collection of rocks, each rock has a positive integer weight.

    Each turn, we choose the two heaviest rocks and smash them together.
    Suppose the stones have weights x and y with x <= y.  The result of this smash is:

    If x == y, both stones are totally destroyed;
    If x != y, the stone of weight x is totally destroyed, and the stone of weight y has new weight y-x.

    At the end, there is at most 1 stone left.  Return the weight of this stone (or 0 if there are no stones left.)
  */
  def lastStoneWeight(stones: Array[Int]): Int = {

    def smash(x: Int, y: Int): Option[Int] = {
      if (x == y)
        None
      else if (x != y)
        Some(Math.abs(x - y))
      else
        None
    }

    /*
      Ordering[U].on(U => T):Ordering[U]   以T作比较标准
      Ordering.by[S,T](S => T):Ordering[T]
      implicit val ord = Ordering[Int].on(x => x).reverse

    */

    implicit val ord = Ordering.by[Int, Int](x => x)

    val pq = mutable.PriorityQueue()
    for (s <- stones)
      pq.enqueue(s)

    while (pq.length >= 2) {
      val x = pq.dequeue()
      val y = pq.dequeue()

      val z = smash(x, y)

      if (z.isDefined)
        pq.enqueue(z.get)
      else {}
    }

    if (pq.isEmpty)
      0
    else
      pq.dequeue()
  }

  //1049
  /*
    Last Stone Weight II
    We have a collection of rocks, each rock has a positive integer weight.

    Each turn, we choose any two rocks and smash them together.
    Suppose the stones have weights x and y with x <= y.  The result of this smash is:

    If x == y, both stones are totally destroyed;
    If x != y, the stone of weight x is totally destroyed, and the stone of weight y has new weight y-x.

    At the end, there is at most 1 stone left.
    Return the SMALLEST possible weight of this stone (the weight is 0 if there are no stones left.)
  */
  def lastStoneWeightII(stones: Array[Int]): Int = {
    /**
      * 2,7,4,1,8,1
      * *
      * 1 8 => 2 7 7 4 1 1
      * *
      * 7 7 => 2 4 1 1
      * *
      * 2 4 => 2 1 1
      * *
      * 2 1 => 1 1
      * *
      * 1 1 => 0
      *
      */

    0
  }

  // 981. Time Based Key-Value Store
  /*
    Create a timebased key-value store class TimeMap, that supports two operations.

    1. set(string key, string value, int timestamp)

    Stores the key and value, along with the given timestamp.

    2. get(string key, int timestamp)

    Returns a value such that set(key, value, timestamp_prev) was called previously, with timestamp_prev <= timestamp.
    If there are multiple such values, it returns the one with the largest timestamp_prev.
    If there are no values, it returns the empty string ("").
   */
  class TimeMap() {

    /** Initialize your data structure here. */


    def set(key: String, value: String, timestamp: Int) {

    }

    def get(key: String, timestamp: Int): String = {

      ""
    }

  }

  /**
    * Your TimeMap object will be instantiated and called as such:
    * var obj = new TimeMap()
    * obj.set(key,value,timestamp)
    * var param_2 = obj.get(key,timestamp)
    */


}
