package limbo.leetcodeSolution

object Array_Solution {
  //1. Two Sum
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    if(nums.length < 2)
      return Array(0,0)
    var map = scala.collection.mutable.Map[Int,Int]()
    var x = 0
    for(i <- nums.indices){
      if(map.contains(nums(i)))
        return Array(map(nums(i)),i)
      val minus = target - nums(i)
      map.+=(minus -> i)
    }
    Array()
  }

  //4. Median of Two Sorted Arrays
  /*
    There are two sorted arrays nums1 and nums2 of size m and n respectively.

    Find the median of the two sorted arrays. The overall run time complexity should be O(log (m+n)).

    You may assume nums1 and nums2 cannot be both empty.

    Example 1:

    nums1 = [1, 3]
    nums2 = [2]

    The median is 2.0
    Example 2:

    nums1 = [1, 2]
    nums2 = [3, 4]

    The median is (2 + 3)/2 = 2.5

        left_part          |        right_part
  A[0], A[1], ..., A[i-1]  |  A[i], A[i+1], ..., A[m-1]
  B[0], B[1], ..., B[j-1]  |  B[j], B[j+1], ..., B[n-1]

  1) len(left_part) == len(right_part)
  2) max(left_part) <= min(right_part)

  (1) i + j == m - i + n - j
    (can be replace by m - i + n - j + 1 , because m - i + n - j + 1 includes
    the odd situation and does not affect the result of divide)
    we need to ensure n >= m, such that j can not be negative

    i + j = (m + n) / 2
    i belongs to [0,m]
    =>  (n + m) / 2 - m <= j <= (m + n) / 2
    =>  (n + m) / 2 - i <= j , same as (n + m + 1) / 2 - i <= j

  (2) B[j-1] <= A[i] and A[i-1] <= B[j]

  Searching i in [0, m], to find an object `i` that:
    B[j-1] <= A[i] and A[i-1] <= B[j], ( where j = (m + n + 1)/2 - i )

  do a binary search following steps described below:

  <1> Set imin = 0, imax = m, then start searching in [imin, imax]

  <2> Set i = (imin + imax)/2, j = (m + n + 1)/2 - i

  <3> Now we have len(left_part)==len(right_part). And there are only 3 situations
       that we may encounter:
      <a> B[j-1] <= A[i] and A[i-1] <= B[j]
          Means we have found the object `i`, so stop searching.
      <b> B[j-1] > A[i]
          Means A[i] is too small. We must `ajust` i to get `B[j-1] <= A[i]`.
          Can we `increase` i?
              Yes. Because when i is increased, j will be decreased.
              So B[j-1] is decreased and A[i] is increased, and `B[j-1] <= A[i]` may
              be satisfied.
          Can we `decrease` i?
              `No!` Because when i is decreased, j will be increased.
              So B[j-1] is increased and A[i] is decreased, and B[j-1] <= A[i] will
              be never satisfied.
          So we must `increase` i. That is, we must ajust the searching range to
          [i+1, imax]. So, set imin = i+1, and goto <2>.
      <c> A[i-1] > B[j]
          Means A[i-1] is too big. And we must `decrease` i to get `A[i-1]<=B[j]`.
          That is, we must ajust the searching range to [imin, i-1].
          So, set imax = i-1, and goto <2>.



    When the object i is found, the median is:
    max(A[i-1], B[j-1]) (when m + n is odd)
    or (max(A[i-1], B[j-1]) + min(A[i], B[j]))/2 (when m + n is even)


    consider the edges values i=0,i=m,j=0,j=n where A[i-1],B[j-1],A[i],B[j] may not exist
    What we need to do is ensuring that max(left_part) <= min(right_part)

    in a searching loop, we will encounter only three situations:

    <a> (j == 0 or i == m or B[j-1] <= A[i]) and
        (i == 0 or j = n or A[i-1] <= B[j])
        Means i is perfect, we can stop searching.

    <b> j > 0 and i < m and B[j - 1] > A[i]
        Means i is too small, we must increase it.

    <c> i > 0 and j < n and A[i - 1] > B[j]
        Means i is too big, we must decrease it.


  */
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    val m = nums1.length
    val n = nums2.length
    if(m > n)
      findMedianSortedArrays(nums2,nums1)

    /*
            left_part          |        right_part
      A[0], A[1], ..., A[i-1]  |  A[i], A[i+1], ..., A[m-1]
      B[0], B[1], ..., B[j-1]  |  B[j], B[j+1], ..., B[n-1]
    */
    var maxOfLeft,minOfRight = 0.0
    val A = nums1
    val B = nums2
    var minIdx = 0
    var maxIdx = m
    var i,j = 0
    val mid = (m + n + 1) >> 1
    var break = false
    //searching for feasible position
    while(minIdx < maxIdx && !break){
      i = (minIdx + maxIdx) >> 1
      j = mid - i
      if(i < m && j > 0 && B(j - 1) > A(i))
        minIdx = i + 1
      else if(i > 0 && j < n && B(j) < A(i - 1))
        maxIdx = i - 1
      else{
        if(i == 0)
          maxOfLeft = B(j - 1)
        else if(j == 0)
          maxOfLeft = A(i - 1)
        else
          maxOfLeft = math.max(A(i - 1),B(j - 1))
        break = true
      }
    }

    //odd situation
    if(((m + n) & 1) == 1)
      return maxOfLeft

    if(i == m) minOfRight = B(j)
    else if(j == n) minOfRight = A(i)
    else minOfRight  = math.min(A(i),B(j))

    (maxOfLeft + minOfRight) / 2.0
  }

  //11 找一个整型数组中的最大区域(柱状图可以“装水”的最大区域面积)
  def maxArea(height: Array[Int]): Int = {
    var maxArea = 0
    var l = 0
    var r = height.length - 1

    // 两侧光标往中间收缩试图找光标可以确定的最大区域
    //
    while (l < r) {
      val y =
        if (height(l) > height(r)) height(r)
        else height(l)
      val x = r - l
      val curArea = x * y

      maxArea = Math.max(maxArea, curArea)

      // 当前指针高度小于另一侧，指针往中心移动
      // 试图找比原先高度更高的点，抵消因移动导致的宽度减小
      if (height(l) < height(r))
        l = l + 1
      else r = r - 1
    }

    maxArea
  }

  def kSumSolution(nums: Array[Int], k: Int, target: Int): List[List[Int]] = {
    /**
      * (K -3)Sum   3Sum持续推进
      * threeSumHead    twoSumStart
      * idx          i                   lo        hi
      * 2Sum_Target = Target - nums(idx) - nums(i)
      * *
      * twoSumStart = threeSumHead + 1
      * *
      * 0 <- idx until nums.length - k - 1
      * *
      * i <- idx + 1 to nums.length - k - 1
      * i先前进找到所有的 3Sum，再从 first3SumPos -1 开始，后退结合成kSum
      *
      */

    def getAns(nums: Array[Int], k: Int, target: Int): List[List[Int]] = {
      val sorted = nums.sorted
      //            println("sorted: " + sorted.toList + " target: " + target)
      if (k == 1)
        return oneSum(sorted, target)

      traverse(List(), sorted, 0, k)
    }

    // -2 -1 0 0 1 2
    // idx <- 0 until sorted.length - k + 1
    def traverse(ans: List[List[Int]], sorted: Array[Int], idx: Int, k: Int): List[List[Int]] = {
      if (idx >= sorted.length - k + 1)
        ans
      else if (idx > 0 && sorted(idx) == sorted(idx - 1)) // 跳过重复
        traverse(ans, sorted, idx + 1, k)
      else {
        //                println("ans: " + ans)
        val curAns = kSum(List(), sorted, idx, idx, sorted.length - 1, k, target, idx)
        //                println("curAns: " + curAns + "\n")
        traverse(ans ::: curAns, sorted, idx + 1, k)
      }
    }

    // i <- idx + 1 to sorted.length - k + 1
    // i先前进找到所有的 3Sum，再从 first3SumPos -1 开始，后退结合成kSum
    def kSum(res: List[List[Int]], nums: Array[Int], idx: Int, i: Int, end: Int, k: Int, target: Int, first3SumPos: Int): List[List[Int]] = {
      if (k == 2) //只求 k == 2
        twoSum(List(), nums, target, 0, 0, nums.length - 1)
      else if (k == 3) {
        // 从k 递归至此 求3Sum
        if (i <= end) {
          //            4
          // -5 -4 -3 1 1 1 2
          if (i > first3SumPos && nums(i) == nums(i - 1)) //跳过重复
            kSum(res, nums, idx, i + 1, end, k, target, first3SumPos)
          else {
            //当前 i 一个可能的3Sum结果，List[List[Int]]
            val r = threeSum(List(), nums, target - nums(i), i, i + 1, nums.length - 1)
            //                        println("r threeSum: " + r)
            //保持idx处的target，此处k 仍为 3 , 第一个3Sum的位置保留
            kSum(res ::: r, nums, idx, i + 1, end, k, target, first3SumPos)
          }
        }
        else
          getkSum(res, nums, idx, first3SumPos - 1) //往回求kSum
      }
      else if (i > idx && nums(i) == nums(i - 1)) // 跳过重复
        kSum(res, nums, idx, i + 1, end, k, target, first3SumPos + 1)
      else if (k > 2 && i < end)
      // i 边往前移动边对target求差，若(k -1)Sum满足，则kSum满足
        kSum(res, nums, idx, i + 1, end, k - 1, target - nums(i), first3SumPos + 1)
      else
        List()
    }

    // 从first3SumPos -1开始，后退结合成kSum
    def getkSum(res: List[List[Int]], nums: Array[Int], idx: Int, i: Int): List[List[Int]] = {
      if (i >= idx) {
        val r = res.map(x => nums(i) :: x)
        getkSum(r, nums, idx, i - 1)
      }
      else if (i > idx && nums(i) == nums(i + 1)) // 跳过重复
        getkSum(res, nums, idx, i - 1)
      else
        res
    }

    def threeSum(res: List[List[Int]], nums: Array[Int], target: Int, threeSumHead: Int, lo: Int, hi: Int): List[List[Int]] = {
      val sum2 = twoSum(res, nums, target, lo, lo, hi)
      val r =
        if (sum2.nonEmpty)
          sum2.map(x => nums(threeSumHead) :: x)
        else
          sum2

      r
    }

    def twoSum(res: List[List[Int]], nums: Array[Int], target: Int, twoSumStart: Int, lo: Int, hi: Int): List[List[Int]] = {
      if (lo >= hi)
        res
      else if (lo > twoSumStart && nums(lo) == nums(lo - 1)) //跳过重复
        twoSum(res, nums, target, twoSumStart, lo + 1, hi)
      else if (hi < nums.length - 1 && nums(hi) == nums(hi + 1))
        twoSum(res, nums, target, twoSumStart, lo, hi - 1)
      //找到一组，继续找
      else if (target - nums(lo) == nums(hi))
        twoSum(res :+ List(nums(lo), nums(hi)), nums, target, twoSumStart, lo + 1, hi - 1) //往内继续找
      else if (nums(lo) + nums(hi) < target)
        twoSum(res, nums, target, twoSumStart, lo + 1, hi)
      else if (nums(lo) + nums(hi) > target)
        twoSum(res, nums, target, twoSumStart, lo, hi - 1)
      else
        List()
    }

    def oneSum(nums: Array[Int], target: Int): List[List[Int]] = {
      val one = nums.find(x => x == target)
      if (one.isDefined)
        List(List(one.get))
      else
        List()
    }

    getAns(nums, k, target)
  }

  //18 List内找4个相加为target的组合
  def fourSum(nums: Array[Int], target: Int): List[List[Int]] = {
    kSumSolution(nums, 4, target)
  }

  //26 去除一个有序Array中的重复元素，处理后返回长度
  def removeDuplicates(nums: Array[Int]): Int = {
    /**
      * 前后指针指向的值不一致，
      * 前值赋值到后指针的下一个位置，顶掉重复
      */
    var count = 0

    if (nums.length < 2)
      return nums.length

    for (i <- 0 to nums.length - 1) {
      if (nums(i) != nums(count)) {
        count = count + 1
        nums(count) = nums(i)
      }
    }

    count + 1
  }

  //27 去除一个array中的指定元素
  def removeElement(nums: Array[Int], v: Int): Int = {
    var len = nums.length

    var i = 0
    while (i < len) {
      val l = len - 1
      if (nums(i) == v) {
        nums(i) = nums(l)
        len = len - 1
      } else
        i = i + 1
    }

    len
  }

  //1053 在一个非负的数组内交换一次两个数的位置，
  // 得到字典序次高的序列，若没有则返回原序列
  def prevPermOpt1(A: Array[Int]): Array[Int] = {
    /**
      * 右往左遍历，找到第一个递减到递增的位置
      * 在此位置右侧找到次小的元素
      */
    if (A.length == 1)
      return A

    var left = A.length - 2
    var right = A.length - 1
    var tmp = 0

    while (A(left - 1) < A(left))
      left = left - 1
    if (left < 0)
      return A

    while (A(right) >= A(left))
      right = right - 1

    while (A(right - 1) == A(right))
      right = right - 1

    tmp = A(right)
    A(right) = A(left)
    A(left) = tmp

    A
  }

  //1052 生气的书店店长
  // i 分钟会有 customer(i)个顾客， grumpy(i)说明店主是否生气
  // 有一次持续 X分钟不生气的机会
  // 求店长不生气情况下的最多顾客数量
  def maxSatisfied(customers: Array[Int], grumpy: Array[Int], X: Int): Int = {
    /**
      * 随下标移动记录满意的人数、不满意的人数，
      * X长度的窗口随下标移动，找到X窗口下最多不满意的人数
      */

    var satisfied = 0
    var grumpyWin = 0
    var max = 0

    for (i <- customers.indices) {
      if (grumpy(i) == 0)
        satisfied = satisfied + customers(i)
      if (grumpy(i) == 1)
        grumpyWin = grumpyWin + customers(i)
      if (i >= X) //窗口开始滑动
        grumpyWin = grumpyWin - customers(i - X) * grumpy(i - X)
      if (grumpyWin > max)
        max = grumpyWin
      else
        max
    }

    max + satisfied
  }

  //406 Array里的每个元素均为 (h,k) 的二元组，
  // h表示自己的身高，k表示此元素之前有几个身高大于等于自身的元素
  def reconstructQueue(people: Array[Array[Int]]): Array[Array[Int]] = {
    /**
      * 身高最高且k小的排前，
      * 身高次高的按K值插入先前的序列
      */

    //    import java.util
    //    util.Arrays.sort(people,
    //    import java.util.Comparator
    //    new Comparator[Array[Int]]() {
    //      override def compare(o1: Array[Int], o2: Array[Int]): Int =
    //        if (o1(0) != o2(0)) -o1(0) + o2(0)
    //        else o1(1) - o2(1)
    //      }
    //    )
    val list = new java.util.LinkedList[Array[Int]]()
    var i = 0
    while (i < people.length) {
      list.add(people(i)(1), people(i))
      i += 1
      i - 1
    }

    list.toArray(people)
  }

  //621 CPU任务调度
  //大写字母代表任务，CPU在每个时间片可以处理也可以idle空闲
  //需要保证每个相同任务的间隔有n个时间片
  //任务不用考虑先后顺序
  //求满足以上条件下的最小时间片数
  def leastInterval(tasks: Array[Char], n: Int): Int = {
    /**
      * A B _ A B _ A B _ A B
      * n = 2
      * *
      * 找出现次数最多的子母，往间隔中插入idle或任务
      * *
      * maybe (maxTaskOccurNum - 1) * (n + 1) + numOfMaxTask
      * A B  _     A B
      * *
      * 一种最多出现次数的子母之间全为idle的情况
      * *
      * Max(tasks.length , maybe)
      * 原task长度大于maybe意味着不需要填充idle
      */

    val countArr = Array.fill(26)(0)
    var singleMaxTaskOccurNum = 0
    var numOfMaxTask = 0
    for (c <- tasks) {
      countArr(c - 'A') += 1
      val cur = countArr(c - 'A')
      if (cur == singleMaxTaskOccurNum)
        numOfMaxTask += 1
      if (cur > singleMaxTaskOccurNum) {
        singleMaxTaskOccurNum = cur
        numOfMaxTask = 1
      }
    }

    val maybe = (singleMaxTaskOccurNum - 1) * (n + 1) + numOfMaxTask
    val res = if (maybe < tasks.length) tasks.length else maybe

    res
  }

}

