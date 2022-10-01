package limbo.leetcodeSolution

import scala.collection.mutable.ListBuffer

object Contest {

  /*
    You are given the array paths, where paths[i] = [cityAi, cityBi] means there exists a direct path going from cityAi to cityBi. Return the destination city, that is, the city without any path outgoing to another city.

    It is guaranteed that the graph of paths forms a line without any loop, therefore, there will be exactly one destination city.

    Input: paths = [["B","C"],["D","B"],["C","A"]]
    Output: "A"
    Explanation: All possible trips are:
    "D" -> "B" -> "C" -> "A".
    "B" -> "C" -> "A".
    "C" -> "A".
    "A".
    Clearly the destination city is "A".
  */
  def destCity(paths: List[List[String]]): String = {
    paths.map(_(1)).filter(ds => paths.forall(p => p.head != ds)).head
  }

  /*
    Given an array nums of 0s and 1s and an integer k, return True if all 1's are at least k places away from each other, otherwise return False.

    Input: nums = [1,0,0,0,1,0,0,1], k = 2
    Output: true
    Explanation: Each of the 1s are at least 2 places away from each other.

    Constraints:

    1 <= nums.length <= 10^5
    0 <= k <= nums.length
    nums[i] is 0 or 1
  */
  def kLengthApart(nums: Array[Int], k: Int): Boolean = {

    def check(i: Int, j: Int): Boolean = {
      if(i == nums.length)
        return true
      if (nums(i) == 1) {
        var u = j
        while (u < nums.length) {
          if (nums(u) == 1) {
            if (u - i - 1 < k)
              return false
            return check(u, u + 1)
          }
          u += 1
        }
      }

      check(i + 1, j + 1)
    }

    check(0,1)
  }


}
