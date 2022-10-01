package limbo.leetcodeSolution

import scala.collection.mutable.ArrayBuffer

object Bits {
  /*
    &
    101
    001
    001

    |

    ^ 异或 不进位的加法
    00 0
    10 1
    01 1
    11 0
    a ^ a = 0
    0 ^ a = a


    ~ 取反

    1101 >> 1 = 110
    1101 << 1 = 11010

    x & -x 求出 x 的二进制表示的最右的一个1
    +1 会将原来最右侧的零恢复回来，进位会得到最右侧的1

    x > 0
    5 = 101

    x < 0
    |x| 101

    x 补码 ~x + 1
    101 的补码 011



  */


  //231 Power of two
  /*
    x & -x = x x是2的整次幂
    x & -x < x x不是2的整次幂

    n > 0 && (1 << 30) % n == 0  Int 最高位 2^31 取余n不剩
  */
  def isPowerOfTwo(n: Int): Boolean = {
    n > 0 && ((n & -n) == n)
  }

  //762. Prime Number of Set Bits in Binary Representation
  /*
    R <= 10^6 (D)
    闭区间[L,R]内的整数的二进制表示中
    1的个数是质数的
    整数个数

    1024^2 > 10^6   不超过20位二进制

    20以下质数
    2 3 5 7 11 13 17 19

    右移记录1的个数
  */
  def countPrimeSetBits(L: Int, R: Int): Int = {
    val primes = Array(2, 3, 5, 7, 11, 13, 17, 19)
    val nums = Stream.range(L, R + 1)
    val whether = nums.map(x => {
      var cur = x
      var count = 0
      while (cur != 0) {
        if ((cur & 1) == 1) //末位是1
          count += 1
        cur = cur >> 1
      }
      primes.contains(count)
    })

    whether.count(x => x)
  }

  //136. Single Number
  /*
    Given a non-empty array of integers,
    every element appears twice except for one.
    Find that single one.

    a ^ a = 0
    0 ^ a = a

    所有数异或运算，相同的数会消除，且满足交换律
  */
  def singleNumber(nums: Array[Int]): Int = {
    nums.foldLeft(0)((a, b) => a ^ b)
  }

  //476. Number Complement
  /*
    Given a positive integer, output its complement number.
    The complement strategy is to flip the bits of its binary representation.

    求反码
    个位开始每一位取反后左移往前推
  */
  def findComplement(num: Int): Int = {
    var cur = num
    var pos = 0
    var res = 0
    while (cur != 0) {
      // ~1 = -2 1111....11111110
      val curRes = if ((cur & 1) == 1) 0 else 1
      res += curRes << pos
      pos += 1
      cur = cur >> 1
    }

    res
  }

  //137. Single Number II
  /*
    Given a non-empty array of integers,
    every element appears three times except for one,
    which appears exactly once. Find that single one

    按位暴搜，
    0th ~ 31th
    当前位累加值 % 3 == 0 不是单独那个数的二进制位
    当前位累加值 % 3 == 1 是单独那个数的二进制位

    状态机:
         ones twos
    初始  0     0

    1个1  1     0
    2个1  0     1
    3个1  0     0

    int ones = 0, twos = 0;
    for(auto x : nums)
    {
      ones = (ones ^ x) & ~twos ;
      twos = (twos ^ x) & ~ones ;
    }
    return ones;


  */
  def singleNumber2(nums: Array[Int]): Int = {
    val bits = Stream.range(0, 32)

    bits.map(x => {
      var curRes = 0
      nums.foreach(y => {
        val curBit = y >> x & 1 //右移要当前数字当前位
        curRes += curBit
      })
      (curRes % 3) << x //左移求单独数字的当前位值
    }).sum
  }

  //260 Single Number 3
  /*
    Given an array of numbers nums,
    in which exactly two elements appear only once
    and all the other elements appear exactly twice.
    Find the two elements that appear only once

    聚合一次的结果是两个不同的数的异或和
    取这个异或和结果的某一位，且此位为1
    根据这一位将整数集合分为两部分
    划分依据为此位异或和为0或为1

    s = a ^ b
    s1 = s ^ S1 = a ^ b ^ a
    s2 = s ^ S2 = a ^ b ^ b

  */
  def singleNumber3(nums: Array[Int]): Array[Int] = {
    /*
    val stream = nums.toStream
    val s = stream.foldLeft(0)((acc,b) => acc ^ b)
    var k = 0 //第k位为1
    while(!((s >> k & 1) == 1))
      k += 1

    val s1 = stream.filter(x => (x >> k & 1) == 1)
        .foldLeft(0)((acc,b) => acc ^ b)

    val s2 = s ^ s1

    Array(s1,s2)
    */

    val stream = nums.toStream
    val s = stream.foldLeft(0)((acc, b) => acc ^ b)

    val k = s & -s
    var x1, x2 = 0

    stream.foreach(x => {
      if ((x & k) == 0)
        x1 = x1 ^ x
      else
        x2 = x2 ^ x
    })

    Array(x1, x2)
  }

  //371. Sum of Two Integers
  /*
    Calculate the sum of two integers a and b,
    but you are not allowed to use the operator + and -.

    异或求出不进位加法
    与运算求出进位，左移后与异或相加
    尾递归不断把进位加入
    直到代表进位的b左移至0

  */
  def getSum(a: Int, b: Int): Int = {
    if (b == 0)
      return a
    val sum = a ^ b
    val carry = (a & b) << 1

    getSum(sum, carry)
  }

  //201. Bitwise AND of Numbers Range
  /*
    Given a range [m, n]
    where 0 <= m <= n <= 2147483647(2^31 - 1),
    return the bitwise AND of all numbers
    in this range, inclusive.

    It's a problem that can be reduced to find the same prefix of the numbers in this range.

    range内的数字某位只要是0，此位与结果肯定是0

    遍历m的每一位，
    若m与n的差距很大，m高位全零，与结果高位全零

    考虑m的第i位，下一个第i位为零且最小的那个数仍在Range内
    该位与结果为0，否则为1
                i
    m = 10110000111

    下一个第i位为零且最小的那个数
        10110001000

        10110000111

        00000000100
     -1 00000000011
     ~
        11111111100
     &  10110000111

        10110000100
     +  00000000100
        10110001000

     0111 .... 11110    2^31 - 2
     0111 .... 11111    2^31 - 1

  */
  def rangeBitwiseAnd(m: Int, n: Int): Int = {
    //    Stream.from(0)
    //      .takeWhile(x => 1 << x <= m) //拿m的所有位
    //      .map(x => {
    //      //m的某一位是1
    //      if((m >> x & 1) == 1){
    //        val larger = (m & ~((1 << x) - 1)) + (1 << x)
    //        if(larger > n) 1 << x else 0
    //      }
    //      else
    //        0
    //    }).sum

    //  It's a problem that can be reduced to find the same prefix of the numbers in this range.
    var diff = 0
    var mm = m
    var nn = n
    while (mm != nn) {
      mm = mm >> 1
      nn = nn >> 1
      diff += 1
    }
    nn << diff
  }

  //477. Total Hamming Distance
  /*
    Hamming Distance 两数的二进制表示中不同的位数

    按位讨论
    一组数中的某位，按0 1 分组
    该位的Hamming Distance是分完组后0的个数乘1的个数

  */
  def totalHammingDistance(nums: Array[Int]): Int = {
    Stream.range(0, 32)
      .map(x => {
        val bits = nums.map(y => {
          val curBit = y >> x & 1
          curBit
        })
        val ones = bits.filter(_ == 1).sum
        val zeros = bits.length - ones

        ones * zeros
      }).sum
  }

  //421. Maximum XOR of Two Numbers in an Array
  /*
    Given a non-empty array of numbers,
    a0, a1, a2, … , an-1, where 0 ≤ ai < 231.

    Find the maximum result of ai XOR aj,
    where 0 ≤ i, j < n.

    Could you do this in O(n) runtime?

    对每一个元素都在数组内找到异或值最大的那个
    意味着需要从高位到低位找到差异最大的那个
    位越高权重越大，
    且最高位的产生的差异大于余下所有位产生的差异

    2^30 > 2^0 + ... + 2^29

    Trie 字典前缀树存每一位
    高位到低位 1 往左走 0 往右走

                 0           Node(0,0)
(边权重)16      /            /
              1            Node(1,0)
        8   /               /     \
            11          Node(2,0)  Node(1,2) -- 24(110)
        4    \               \        \
              110           Node(0,3)   ...
        2       \             \
               1100           Node(0,4)
        1      /              /
             11001        Node(5,0)

       16 + 8 + 1 = 25

    2^0 + 2^1 + ... + 2^30 个节点

    构建好以后，
    找每个x对应可以异或值最大的那个
    对于每一个x,每次往x路径的不同方向走

  */
  def findMaximumXOR(nums: Array[Int]): Int = {
    case class Node(son: Array[Int]) {
      override def toString: String = son.map(_.toString).reduce(_ + _)
    }
    val nodes = ArrayBuffer[Node]()
    nodes += Node(Array(0, 0)) //root
    //construct
    nums.foreach(x => {
      var pos = 0
      (0 to 30).reverse.foreach(i => {
        val curBit = x >> i & 1
        // curBit of the number has no node in trie
        if (nodes(pos).son(curBit) == 0) {
          //add an empty node for next bit
          nodes += Node(Array(0, 0))
          //current son get the latest path of the number
          nodes(pos).son(curBit) = nodes.length - 1
        }
        // pos get ready for next bit
        pos = nodes(pos).son(curBit)
      })
    })

    // fine the correspond number of current number that can get the Max XOR
    val xors = nums.map(x => {
      var pos, xor = 0
      //greedy top-down by bits ()
      (0 to 30).reverse.foreach(i => {
        val curBit = x >> i & 1
        val rev = if (curBit == 0) 1 else 0
        // curBit of current number can find the reverse bit from nums
        if (nodes(pos).son(rev) != 0) {
          //go to next iteration by another path
          pos = nodes(pos).son(rev)
          xor += 1 << i
        }
        else
        // can not find , go to next iteration by original path
          pos = nodes(pos).son(curBit)
      })

      xor
    })

    xors.max
  }

}
