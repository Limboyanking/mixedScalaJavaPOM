package limbo.leetcodeSolution

object Binary_MonotonicQueue {

  //239.Sliding Window Maximum
  /*
   Given an array nums, there is a sliding window of size k which is moving from the very left of the array to the very right.
   You can only see the k numbers in the window. Each time the sliding window moves right by one position.
   Return the max sliding window.

   Example:

   Input: nums = [1,3,-1,-3,5,3,6,7], and k = 3
   Output: [3,3,5,5,6,7]
   Explanation:

   Window position                Max
   ---------------               -----
   [1  3  -1] -3  5  3  6  7       3
    1 [3  -1  -3] 5  3  6  7       3
    1  3 [-1  -3  5] 3  6  7       5
    1  3  -1 [-3  5  3] 6  7       5
    1  3  -1  -3 [5  3  6] 7       6
    1  3  -1  -3  5 [3  6  7]      7


    位置在窗口内靠后的位置且比窗口内靠前的大
                           |
    --------|------------|-|-|--------
            a                b



           tail            head
    用队列维护，只要队头比当前元素小，剔除队头
    直到队列为空，或者是第一个比当前元素大的，插入队头
    每次插入新元素都在队内把比自身小的元素从队头剔除
    保证队尾是当前窗口内的最大，且保证tail至head始终保持单调递减
    每次移动窗口看队尾元素是否出窗，出窗则从队尾踢出

    用双向队列，队首队尾都可以出队

  */
  def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
    // idx
    var deque = scala.collection.mutable.ArrayBuffer[Int]()
    val res = scala.collection.mutable.ArrayBuffer[Int]()
    var j = 0
    for(i <- nums.indices){
      //kill all the numbers in deque which less than num(i)
      while(deque.nonEmpty && nums(i) > nums(deque(0)))
        deque = deque.slice(1,deque.length)
      //add current i
      //leads to a Monotonic queue
      //always make last one be the greatest one in the window
      deque = i +: deque

      //deque move forward corresponding to window
      //also get rid of previous max
      if(i - j + 1 > k){
        if(deque.last <= j)
          deque = deque.slice(0,deque.length - 1)
        j += 1
      }

      if(i - j + 1 == k)
        res += nums(deque.last)
    }

    res.toArray
  }

}
