package limbo.leetcodeSolution

import scala.collection.immutable.Queue
import scala.collection.{SeqLike, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object DFS_BFS {

  /*
    2^n 规模的问题

    BFS O(2^n)
    1.空间是指数级别
    2.不会有爆栈风险
    3.有最小性质，解决最短路径问题

    DFS O(n)
    1.空间与深度成正比
    2.不需要显式的维护栈，小心爆栈
    3.不能搜最短、最小

  */

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = _
    var right: TreeNode = _
  }

  //111. Minimum Depth of Binary Tree
  /*
    Given a binary tree, find its minimum depth.
    The minimum depth is the number of nodes
   along the shortest path
    from the root node down to the nearest leaf node
  */
  def minDepth(root: TreeNode): Int = {
    if (root == null)
      return 0
    val left = minDepth(root.left)
    val right = minDepth(root.right)
    if (left == 0 || right == 0)
      return left + right + 1 //加上父节点

    Math.min(left, right) + 1
  }

  //279. Perfect Squares
  /*
    Given a positive integer n,
    find the least number of perfect square numbers
    (for example, 1, 4, 9, 16, ...) which sum to n.

    0点到n点的最短距离

    从0点开始，每次加上一个平方数可以转移到另外一个点

    0 + 4
    0 + 9
    两个操作到0的距离都是1

                   0
                 /
                1  4  9  16 ....
                |
                1
                |
                1  4  9  16 ....
                 \
                   2
                    \
                1  4  9  16 ....

                     12 最左的一条路径与到达当前层的任意一条路径距离相同

    queue.front 保存当前路径下的累计和
    val dist = Array.fill(n + 1)(Integer.maxValue)保存到各节点的最短距离

    若当前路径下累计和t等于n 返回dist(t)

    queue.front 在每层从左至右尝试，每种可能的路径都要入队
    下一个可能的节点 t + i * i
    if dist(t + i * i) > dist(t) + 1
      dist(t + i * i) = dist(t) + 1
    更新距离以后，将当前路径的累计和入队

    0 ~ n 视为图的 n + 1 个点，
    若一个点加平方数可以到另一个点，视为点之间可以用边进行连接

  */
  def numSquares(n: Int): Int = {
    var queue = Queue(0)
    val dist = Array.fill(n + 1)(Integer.MAX_VALUE)
    dist(0) = 0

    //不断宽度搜索
    while (queue.nonEmpty) {
      val curSum = queue.head
      queue = queue.dequeue._2

      //当前搜索路径到达n
      if (curSum == n)
        return dist(n)

      var i = 1
      //下一步不超过n
      while (i * i + curSum <= n) {
        val nextNode = i * i + curSum
        if (dist(nextNode) > dist(curSum) + 1) {
          //更新最短距离
          dist(nextNode) = dist(curSum) + 1
          //压入当前路径累计值
          queue = queue.enqueue(nextNode)
        }
        i += 1
      }
    }

    0
  }

  //733. Flood Fill
  /*
    An image is represented by a 2-D array of integers,
    each integer representing the pixel value of the image (from 0 to 65535).

    Given a coordinate (sr, sc) representing the starting pixel
    (row and column) of the flood fill, and a pixel value newColor,
    "flood fill" the image.

    To perform a "flood fill",
    consider the starting pixel,
    plus any pixels connected 4-directionally to
    the starting pixel of the same color as the starting pixel,
    plus any pixels connected 4-directionally to those pixels
    (also with the same color as the starting pixel), and so on.
    Replace the color of all of the aforementioned pixels with the newColor.

    At the end, return the modified image.

    1 1 1     2 2 2
    1 1 0  => 2 2 0
    1 0 0     2 0 0

  */
  def floodFill(image: Array[Array[Int]], sr: Int, sc: Int, newColor: Int): Array[Array[Int]] = {
    // 上 右 下 左 向量
    val dx = Array(-1, 0, 1, 0)
    val dy = Array(0, 1, 0, -1)

    val oldColor = image(sr)(sc)
    if (oldColor == newColor) //防止往回涂色，死循环
    return image
    image(sr)(sc) = newColor //涂色

    (0 to 3).foreach(i => {
      val x = sr + dx(i)
      val y = sc + dy(i)
      if (x >= 0 && x < image.length
        && y >= 0 && y < image(0).length
        && image(x)(y) == oldColor) //走到下一个不同色的格子
      floodFill(image, x, y, newColor)
    })

    image
  }

  //200. Number of Islands
  /*
    Given a 2d grid map of '1's (land) and '0's (water),
    count the number of islands. An island is surrounded by water
    and is formed by connecting adjacent lands horizontally or vertically.
    You may assume all four edges of the grid are all surrounded by water.

    遍历矩阵，碰到1开始dfs，将当前岛的区域置0
    整体统计需要dfs操作的数量就是岛的数量

  */
  def numIslands(grid: Array[Array[Char]]): Int = {
    if (grid.isEmpty || grid(0).isEmpty)
      return 0

    val m = grid.length
    val n = grid(0).length
    var res = 0

    for (i <- 0 until m) {
      for (j <- 0 until n) {
        if (grid(i)(j) == '1') {
          res += 1
          dfs(grid, i, j)
        }
      }
    }

    def dfs(grid: Array[Array[Char]], i: Int, j: Int): Unit = {
      val dx = Array(-1, 0, 1, 0)
      val dy = Array(0, 1, 0, -1)
      grid(i)(j) = '0'
      (0 to 3).foreach(o => {
        val x = i + dx(o)
        val y = j + dy(o)
        if (x >= 0 && x < grid.length
          && y >= 0 && y < grid(0).length
          && grid(x)(y) == '1')
          dfs(grid, x, y)
      })
    }

    res
  }

  //130. Surrounded Regions
  /*
    Given a 2D board containing 'X' and 'O' (the letter O),
    capture all regions surrounded by 'X'.

    A region is captured by flipping all 'O's into 'X's
    in that surrounded region.

    X X X X      X X X X
    X O O X      X X X X
    X X X O   => X X X O
    X O X X      X O X X
    X O O X      X O O X

    四个边界dfs找O，标记成Y，除了标记为Y的，其余都被X包围

  */
  def solve(board: Array[Array[Char]]): Unit = {
    if (board.isEmpty || board(0).isEmpty)
      return

    val m = board.length
    val n = board(0).length

    for (i <- 0 until m) {
      if (board(i)(0) == 'O')
        dfs(board, i, 0)
      if (board(i)(n - 1) == 'O')
        dfs(board, i, n - 1)
    }

    for (j <- 0 until n) {
      if (board(0)(j) == 'O')
        dfs(board, 0, j)
      if (board(m - 1)(j) == 'O')
        dfs(board, m - 1, j)
    }

    def dfs(board: Array[Array[Char]], i: Int, j: Int): Unit = {
      val dx = Array(-1, 0, 1, 0)
      val dy = Array(0, 1, 0, -1)
      board(i)(j) = 'Y'
      (0 to 3).foreach(k => {
        val x = i + dx(k)
        val y = j + dy(k)
        if (x >= 0 && x < board.length
          && y >= 0 && y < board(0).length
          && board(x)(y) == 'O')
          dfs(board, x, y)
      })
    }

    for (i <- 0 until m) {
      for (j <- 0 until n) {
        board(i)(j) match {
          case 'Y' => board(i)(j) = 'O'
          case 'O' => board(i)(j) = 'X'
          case _ =>
        }
      }
    }

  }

  //543. Diameter of Binary Tree
  /*
    Given a binary tree,
    you need to compute the length of the diameter of the tree.
    The diameter of a binary tree is the length of the longest path
    between any two nodes in a tree.
    *** This path may or may not pass through the root. ***

    最长路径可能不过根节点

    自底向上遍历每一个
       o
     /   \
    o --- o   结构，返回当前结构深度
    更新最长路径

  */
  def diameterOfBinaryTree(root: TreeNode): Int = {
    if (root == null)
      return 0

    var res = 0

    def dfs(root: TreeNode): Int = {
      if (root == null)
        return 0

      //all the way down to the leafs then up for counting
      val leftDepth = dfs(root.left)
      val rightDepth = dfs(root.right)

      //updating res while counting up
      res = Math.max(res, leftDepth + rightDepth + 1)

      //up for counting
      1 + Math.max(leftDepth, rightDepth)
    }

    dfs(root)
    res - 1
  }

  //127. Word Ladder
  /*
    Given two words (beginWord and endWord), and a dictionary's word list,
    find the length of shortest transformation sequence from beginWord to endWord, such that:

    Only one letter can be changed at a time.
    Each transformed word must exist in the word list. Note that beginWord is not a transformed word.

    Note:
      Return 0 if there is no such transformation sequence.
      All words have the same length.
      All words contain only lowercase alphabetic characters.
      You may assume no duplicates in the word list.
      You may assume beginWord and endWord are non-empty and are not the same.

    在wordList集合内，beginWord 可以有几步转换得到 endWord
    每次转换只能变动一个字母
    且每次转换的值都要在wordList集合内

    BFS
    初始化begin 到达 wordList每一元素的距离为 Integer.MAX_VALUE
    queue.front 为当前路径的单词

    返回 dist(endWord)

    双向:
      var len = 1
      beginSet
      endSet
      visited:Vector[String]

      if(beginSet.length > endSet.length) swap beginSet endSet

      for(word <- beginSet){
        val target = curWord change each letter
        if(endSet.contains(target)) // meet point
          return len + 1

        if (!visited.contains(target) && wordList.contains(target)) {
						beginSet = beginSet :+ target
						visited.add(target);
					}

      }

      len += 1



  */
  def ladderLength(beginWord: String, endWord: String, wordList: List[String]): Int = {
    var queue = scala.collection.immutable.Queue(beginWord)
    val ini = wordList.toStream.map((_, Integer.MAX_VALUE))
    var dist = Map(ini: _*) + (beginWord -> 1)

    while (queue.nonEmpty) {
      val curWord = queue.dequeue._1
      val curDist = dist.getOrElse(curWord, Integer.MAX_VALUE)
      queue = queue.dequeue._2

      if (curWord == endWord)
        return dist(endWord)

      //the possible path that curWord can go
      val chs = curWord.toCharArray
      ('a' to 'z').foreach(letter => {
        for (i <- 0 until chs.length) {
          val oldChar = chs(i)
          chs(i) = letter
          val nextNode = chs.foldLeft("")(_ + _)
          //if nextNode in Map and the distance can be updated
          dist.get(nextNode).foreach(d => {
            if (d > curDist + 1) {
              dist = dist + (nextNode -> (curDist + 1))
              // ready for next iteration
              queue = queue.enqueue(nextNode)
            }
          })
          //ready for next letter
          chs(i) = oldChar
        }
      })
    }

    0
  }

  //542. 01 Matrix
  /*
    Given a matrix consists of 0 and 1,
    find the distance of the nearest 0 for each cell(element).

    The distance between two adjacent cells is 1.

    非零元素全部置为 Integer.MaxValue
    每个0元素 BFS 到非零元素后更新最短距离

  */
  def updateMatrix(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    var queue = scala.collection.immutable.Queue[(Int, Int)]()
    val dist = matrix.map(x => {
      x.map {
        case 0 => 0
        case _ => Integer.MAX_VALUE
      }
    })

    val rows = matrix.length
    val cols = matrix(0).length

    for (r <- 0 until rows) {
      for (c <- 0 until cols) {
        if (matrix(r)(c) == 0)
          queue = queue.enqueue((r, c))
      }
    }

    val dx = Array(-1, 0, 1, 0)
    val dy = Array(0, 1, 0, -1)

    while (queue.nonEmpty) {
      val curPos = queue.dequeue._1
      val x = curPos._1
      val y = curPos._2
      queue = queue.dequeue._2

      (0 to 3).foreach(i => {
        val curX = x + dx(i)
        val curY = y + dy(i)
        if (curX >= 0 && curX < matrix.length
          && curY >= 0 && curY < matrix(0).length
          && dist(curX)(curY) > dist(x)(y) + 1) {
          // update distance
          dist(curX)(curY) = dist(x)(y) + 1
          queue = queue.enqueue((curX, curY))
        }
      })
    }

    dist
  }

  //207. Course Schedule
  /*
    There are a total of n courses you have to take, labeled from 0 to n-1.

    Some courses may have prerequisites,
    for example to take course 0 you have to first take course 1,
    which is expressed as a pair: [0,1]

    Given the total number of courses and a list of prerequisite pairs,
    is it possible for you to finish all courses?

    Input: 2, [[1,0]]
    Output: true
    Explanation: There are a total of 2 courses to take.
                 To take course 1 you should have finished course 0. So it is possible

    Input: 2, [[1,0],[0,1]]
    Output: false
    Explanation: There are a total of 2 courses to take.
                 To take course 1 you should have finished course 0,
                 and to take course 0 you should
                 also have finished course 1. So it is impossible.

    有向无环图拓扑排序

    BFS:

    将入度为0的顶点m（如果当前顶点包含关联的边，同时去除其关联边）从图G中取出，
    则剩余的G'依然是有向无环图。
    重新考量图G'，重复1步骤，直到所有顶点均被取出。
    对于每一个取出的顶点m，按取出的先后顺序排列，即构成了G的拓扑排序。

    (1) 按prerequisites连线，初始化各点的入度
    (3) 入度为0的点全部入队(可能的开始节点),
        从入度为0的点开始BFS，没有入度为0的点直接返回false
    (4) 每次连接可能的下一个点时，将可能的下一个点入度 -1 ，若发现新的入度为零的点，入队
    (5) 每次连接记录是否访问到
    (6) 全部访问到则返回true

----------------------------------------------------------------
DFS:
For DFS, in each visit, we start from a node and keep visiting its neighbors,
if at a time we return to a visited node, there is a cycle.
Otherwise, start again from another unvisited node and repeat this proce

    5
    *
    |
    4 * - 3
     \   *
      * /
 1 - * 2
                                 visited     memo
     false      dfs(1)           false       true
      *           |               *            *
      |           |               |            |
     false      dfs(2)           false      true
      *           |               *           *
      |           |               |           |
     false      dfs(3)           false      true
      *           |               *           *
      |           |               |           |
     false      dfs(4)           false      true
      *         /   \             *           *
      |        /     \            |           |
    false  dfs(2)    dfs(5)      false      true
           circle    terminal


// check whether the current path's vertex has been visited
val visited = Array.fill(numCourses)(false)
// record whether the current vertex has been judged
val memo = Array.fill(numCourses)(false)

        private boolean dfs(ArrayList[] graph, boolean[] visited, int course, boolean[] memo){
            if (visited[course]) {
                return false;
            }

            if (memo[course]) {
                return true;
            }
            visited[course] = true;
            for (int i = 0; i < graph[course].size(); i++) {
                if (!dfs(graph, visited, (int)graph[course].get(i), memo)) {
                    return false;
                }
            }
            visited[course] = false;
            memo[course] = true;
            return true;
        }

  */
  def canFinish(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean = {
    var graph = Map[Int, Array[Int]](
      (0 until numCourses)
        .map((_, Array[Int]())): _*
    )
    val visited = Array.fill(numCourses)(false)
    val inDegrees = Array.fill(numCourses)(0)
    var queue = Queue[Int]()

    //generate graph and inDegrees
    for (i <- prerequisites.indices) {
      val dad = prerequisites(i)(1)
      val son = prerequisites(i)(0)
      graph = graph + (dad -> graph(dad).:+(son))
      inDegrees(son) += 1
    }

    //enqueue the vertexes whose inDegree equals 0
    for (i <- inDegrees.indices) {
      if (inDegrees(i) == 0)
        queue = queue.enqueue(i)
    }

    //bfs
    while (queue.nonEmpty) {
      val curVertex = queue.dequeue._1
      queue = queue.dequeue._2

      // circle occur
      if (visited(curVertex))
        return false

      visited(curVertex) = true

      //remove the inDegrees of vertexes which related to curVertex
      val related = graph(curVertex)
      related.foreach(r => {
        inDegrees(r) -= 1
      })

      //ready for next iteration
      related.foreach(r => {
        if (inDegrees(r) == 0)
          queue = queue.enqueue(r)
      })
    }

    visited.forall(x => x)
  }

  //210. Course Schedule II
  /*
   There are a total of n courses you have to take, labeled from 0 to n-1.

   Some courses may have prerequisites, for example to take course 0 you have to first take course 1,
   which is expressed as a pair: [0,1]

   Given the total number of courses and a list of prerequisite pairs,
   return the ordering of courses you should take to finish all courses.

   There may be multiple correct orders, you just need to return one of them.
   If it is impossible to finish all courses, return an empty array.

  */
  def findOrder(numCourses: Int, prerequisites: Array[Array[Int]]): Array[Int] = {
    var res = Array[Int]()
    val graph = Array.fill(numCourses)(Array[Int]())
    val visited = Array.fill(numCourses)(false)
    val indegrees = Array.fill(numCourses)(0)
    var queue = scala.collection.immutable.Queue[Int]()

    //generate graph and indegrees
    prerequisites.foreach(pair => {
      val dad = pair(1)
      val son = pair(0)
      graph(dad) = graph(dad) :+ son
      indegrees(son) += 1
    })

    //enqueue the vertexes that indegree equals 0
    for (i <- indegrees.indices) {
      if (indegrees(i) == 0)
        queue = queue.enqueue(i)
    }

    //BFS
    while (queue.nonEmpty) {
      val curVertex = queue.dequeue._1
      //circle occurs
      if (visited(curVertex))
        return Array()

      visited(curVertex) = true

      //update res then dequeue
      res = res :+ curVertex
      queue = queue.dequeue._2

      //remove the inDegrees of vertexes that related to curVertex
      val related = graph(curVertex)
      related.foreach(v => {
        indegrees(v) -= 1
        //ready for next iteration
        if (indegrees(v) == 0)
          queue = queue.enqueue(v)
      })
    }

    //check if all the vertexes been visited
    if (!visited.forall(x => x))
      return Array()

    res
  }

  //784. Letter Case Permutation
  /*
    Given a string S, we can transform every letter individually to be
    lowercase or uppercase to create another string.
    Return a list of all possible strings we could create.

    Examples:
    Input: S = "a1b2"
    Output: ["a1b2", "a1B2", "A1b2", "A1B2"]

    Input: S = "3z4"
    Output: ["3z4", "3Z4"]

    Input: S = "12345"
    Output: ["12345"]

    DFS backTrack



  */
  def letterCasePermutation(S: String): List[String] = {
    if (S == null)
      return List()

    //change a bit can lead to a set of strings
    //toggle every string in current set of strings
    val ans = scala.collection.mutable.ListBuffer[String](S)
    for (i <- 0 until S.length) {
      for (j <- ans.indices) {
        val cur = ans(j).toCharArray
        if (cur(i) >= 'A') {
          cur(i) = (cur(i).toInt ^ 32).toChar
          val rs = cur.foldLeft("")(_ + _)
          ans += rs
        }
      }
    }

    ans.toList

    //    val res = ListBuffer[String]()
    //
    //    def dfs(s: String, idx: Int): Unit = {
    //      if (idx == s.length) {
    //        // save s when complete a new result
    //        res += s
    //        return
    //      }
    //
    //      //a path that doesn't change current digit or letter
    //      dfs(s, idx + 1)
    //
    //      //a path that will change current letter
    //      if (s(idx) >= 'A') {
    //        var r = s.toCharArray
    //        // 32 10000
    //        // 'A' 65 'a' 97
    //        // r(idx).toInt ^= 32
    //        if (r(idx) >= 'a')
    //          r(idx) = r(idx).toUpper
    //        else r(idx) = r(idx).toLower
    //        val rs = r.foldLeft("")(_ + _)
    //        dfs(rs, idx + 1)
    //      }
    //    }
    //
    //    dfs(S, 0)
    //    res.toList
  }

  //77. Combinations
  /*
    Given two integers n and k,
    return all possible combinations of k numbers out of 1 ... n.

    Example:

    Input: n = 4, k = 2
    Output:
    [
      [2,4],
      [3,4],
      [2,3],
      [1,2],
      [1,3],
      [1,4],
    ]

    Ascending order 1 ... n
    i++ k--
    backTrack 回溯

                                comb     i   k
                                List() , 1 , 2
                          /     |           \         \
                         /      |            \         \
              List(1),2,1  List(2),3,1   List(3),4,1    List(4),5,1
              |
              |
          List(1,2),3,0  List(1,3),4,0  List(1,4),5,0
            return(go up)
          remove last => List(1) for next possible result ( List(1,3) )
            ...

  * */
  def combine(n: Int, k: Int): List[List[Int]] = {
    val ans = ListBuffer[List[Int]]()
    val temp = ListBuffer[Int]()

    def dfs(start: Int, k: Int): Unit = {
      //down to bottom
      //form a possible result
      if (k == 0) {
        ans += temp.toList
        //execution stream direction go up
        return
      }


      //spread out for possible results
      for (i <- start to n) {
        //add current digit to current path
        temp += i

        //go further at current level
        dfs(i + 1, k - 1)

        //if down to bottom
        //back to previous status of temp for next possible result
        temp.remove(temp.length - 1)
      }

    }

    dfs(1, k)
    ans.toList
  }

  //257. Binary Tree Paths
  /*
      Given a binary tree, return all root-to-leaf paths.

      Note: A leaf is a node with no children.

    Example:

    Input:

       1
     /   \
    2     3
     \
      5

    Output: ["1->2->5", "1->3"]

    Explanation: All root-to-leaf paths are: 1->2->5, 1->3

  */
  def binaryTreePaths(root: TreeNode): List[String] = {
    val ans = scala.collection.mutable.ListBuffer[String]()
    val path = ""

    def dfs(root: TreeNode, path: String): Unit = {
      var p = path

      //down to bottom , execution stream direction go up
      if (root == null)
        return

      //dual with very beginning
      //link current node (->current)
      if (p.length > 0)
        p = p + "->"

      p = p + s"${root.value}"

      //down to leaves i.e form a result
      if (root.left == null && root.right == null)
        ans += p

      //spread out
      dfs(root.left, p)

      dfs(root.right, p)
    }

    dfs(root, path)

    ans.toList
  }

  //93. Restore IP Addresses
  /*
    Given a string containing only digits, restore it by returning
    all possible valid IP address combinations.

    Example:

    Input: "25525511135"
    Output: ["255.255.11.135", "255.255.111.35"]

    ascending 1 until s.length
    for each digit of current idx,
    search for possible ip term of current path by
    i <- idx until s.length
    once form an ip term , entail current term to path
    go further by dfs(i + 1,termNum + 1,path) for current path.

    if path goes to illegal ,
    like current term looks .01 .256 or termNum > 4
    return ( execution stream direction goes up ) for discard current situation

    if termNum == 4 && idx == s.length
    record current path
    return


  */
  def restoreIpAddresses(s: String): List[String] = {
    val ans = ListBuffer[String]()

    def dfs(s: String, idx: Int, termNum: Int, path: String): Unit = {
      if (termNum == 4) {
        //be care of all the digits should be used
        // *.*.*.*
        if (idx == s.length && s.length + 3 == path.length - 1)
          ans += path.substring(1)
        return
      }

      //illegal situation
      if (termNum > 4)
        return

      //search for a term
      var term = 0
      for (i <- idx until s.length) {
        if (s(i) == '0' && term == 0)
        //no way to ".0*"
        dfs(s, i + 1, termNum + 1, path + ".0")
          else {
          term = term * 10 + s(i) - '0'
          //no way to ".256"
          if (term > 255)
            return
          //go further
          else
            dfs(s, i + 1, termNum + 1, path + s".${term}")
        }
      }
    }

    dfs(s, 0, 0, "")

    ans.toList
  }

  //95. Unique Binary Search Trees II
  /*
    Given an integer n, generate all structurally unique
    BST's (binary search trees) that store values 1 ... n
    in mid-order traversal

    Example:

    Input: 3
    Output:
    [
      [1,null,3,2],
      [3,2,null,1],
      [3,1,null,null,2],
      [2,1,3],
      [1,null,2,null,3]
    ]
    Explanation:
    The above output corresponds to the 5 unique BST's shown below:

       1    1             2            3      3
        \    \           / \          /      /
         3    2         1   3        2      1
        /      \                    /        \
       2        3                  1          2

   返回中根遍历是1 ... n的所有树

   Any range should conduct a Set of BSTs
   for any digit of l ... r
   recursively searching for BSTs of subRanges

   one BST of current digit should be consist of
   one of the BST within left subTrees
   and one of the BST within right subTrees

   currentDigitBSTSet = leftBSTs * rightBSTs

  */
  def generateTrees(n: Int): List[TreeNode] = {
    if (n == 0)
      return List()

    def dfs(l: Int, r: Int): ListBuffer[TreeNode] = {
      val curBSTs = ListBuffer[TreeNode]()

      //path goes to null , should be recorded then go up
      if (l > r) {
        curBSTs += null

        curBSTs
      }
      else {
        //recursively down to get subRanges's BSTs
        for (i <- l to r) {
          val leftBSTs = dfs(i - 1, l)
          val rightBSTs = dfs(i + 1, r)
          //get current digit's BSTs
          for (lt <- leftBSTs) {
            for (rt <- rightBSTs) {
              //form a BST which belongs to current digit
              val node = new TreeNode(i)
              node.left = lt
              node.right = rt
              //add a new tree of current digit to current range
              curBSTs += node
            }
          }
        }

        curBSTs
      }
    }

    dfs(1, n).toList
  }

  //394. Decode String
  /*
    Given an encoded string, return its decoded string.

    The encoding rule is: k[encoded_string], where the encoded_string inside the square brackets is being repeated exactly k times. Note that k is guaranteed to be a positive integer.

    You may assume that the input string is always valid; No extra white spaces, square brackets are well-formed, etc.

    Furthermore, you may assume that the original data does not contain any digits and that digits are only for those repeat numbers, k. For example, there won't be input like 3a or 2[4].

    Examples:

    s = "3[a]2[bc]", return "aaabcbc".
    s = "3[a2[c]]", return "accaccacc".
    s = "2[abc]3[cd]ef", return "abcabccdcdcdef".
    s = "2[3[a]]" , return "aaaaaa".

    considering the pattern (no endless recurse)

    [0-9]+
    \[
      [a-z]+  exists or not
      [0-9]+  must exists
      \[
      [a-z]+
      \]
    \]

    traversing String s

    when [0-9]+\[ occurs ,
    i,j form a range which
    starts from current "[" to the correspond "]"

    dfs tree goes down , recursively searching for result of
    current range
    i.e "[...]"

    when current range get done,
    add to the answer repeatedly for k times
    i,j move forward


  * */
  def decodeString(s: String): String = {
    val ans = mutable.StringBuilder.newBuilder
    var i, j = 0
    while (i < s.length && j < s.length) {
      //deal with letters
      if (s(i) >= 'A' && s(i) <= 'z') {
        ans.append(s(i))
        i += 1
      }
      else {
        //deal with digits
        var k = 0
        while (s(i).isDigit) {
          k = k * 10 + s(i) - '0'
          i += 1
        }

        //deal with '['
        //find the symmetrical ']'
        var sum = 1
        j = i + 1
        while (sum > 0) {
          if (s(j) == '[') sum += 1
          if (s(j) == ']') sum -= 1
          j += 1
        }

        //deal with ']'
        // i i + 1    j - 1   j
        // [           ]
        val curRes = decodeString(s.substring(i + 1, j - 1))
        while (k > 0) {
          ans.append(curRes)
          k -= 1
        }
        i = j

      }
    }

    ans.toString()
  }

  //341. Flatten Nested List Iterator
  /*
    Given a nested list of integers, implement an iterator to flatten it.

    Each element is either an integer, or a list -- whose elements may also be integers or other lists.

    Example 1:

    Input: [[1,1],2,[1,1]]
    Output: [1,1,2,1,1]
    Explanation: By calling next repeatedly until hasNext returns false,
                 the order of elements returned by next should be: [1,1,2,1,1].
    Example 2:

    Input: [1,[4,[6]]]
    Output: [1,4,6]
    Explanation: By calling next repeatedly until hasNext returns false,
               the order of elements returned by next should be: [1,4,6].
  */
  /**
    * // This is the interface that allows for creating nested lists.
    * // You should not implement it, or speculate about its implementation
    */
  class NestedInteger {
    // Return true if this NestedInteger holds a single integer,rather than a nested list.
    def isInteger: Boolean = ???

    // Return the single integer that this NestedInteger holds,if it holds a single integer
    def getInteger: Int = ???

    // Set this NestedInteger to hold a single integer.
    def setInteger(i: Int): Unit = ???

    // Return the nested list that this NestedInteger holds, if it holds a nested list
    def getList: List[NestedInteger] = ???

    // Set this NestedInteger to hold a nested list and adds a nested integer to it.
    def add(ni: NestedInteger) = ???
  }

  class NestedIterator(_nestedList: List[NestedInteger]) {
    val seq = ListBuffer[Int]()
    var index = 0
    lazy val FormNestedIterator: Unit = dfs(_nestedList)

    def dfs(nestedList: SeqLike[NestedInteger, _]): Unit = {
      for (n <- nestedList) {
        n match {
          case x if x.isInteger => seq += x.getInteger
          case y => dfs(y.getList)
        }
      }
    }

    def isEmpty(): Boolean = seq.isEmpty

    def next() = {
      FormNestedIterator
      index += 1
      seq(index - 1)
    }

    def nextOption(): Option[Int] = {
      FormNestedIterator
      if (this.hasNext)
        Some(this.next)
      else
        None
    }

    def hasNext(): Boolean = {
      FormNestedIterator
      index < seq.length
    }

  }

  //756. Pyramid Transition Matrix AirB&B
  /*
    We are stacking blocks to form a pyramid. Each block has a color which is a one letter string.

    We are allowed to place any color block C on top of two adjacent blocks of colors A and B, if and only if ABC is an allowed triple.

    We start with a bottom row of bottom, represented as a single string. We also start with a list of allowed triples allowed. Each allowed triple is represented as a string of length 3.

    Return true if we can build the pyramid all the way to the top, otherwise false.

    Example 1:

    Input: bottom = "BCD", allowed = ["BCG", "CDE", "GEA", "FFF"]
    Output: true
    Explanation:
    We can stack the pyramid like this:
        A
       / \
      G   E
     / \ / \
    B   C   D

    We are allowed to place G on top of B and C because BCG is an allowed triple.  Similarly, we can place E on top of C and D, then A on top of G and E.

    Note:

    bottom will be a string with length in range [2, 8].

    allowed will have length in range [0, 200].

    Letters in all strings will be chosen from the set {'A', 'B', 'C', 'D', 'E', 'F', 'G'}.

  */
  def pyramidTransition(bottom: String, allowed: List[String]): Boolean = {
    val allowedRelation = Array.fill(7)(Array.fill(7)(ListBuffer[Char]()))
    for (a <- allowed) {
      val row = a(0) - 'A'
      val column = a(1) - 'A'
      val value = a(2)

      allowedRelation(row)(column) += value
    }

    //preLayer + curLayer can form a possible path
    def dfs(preLayer: String, curLayer: String): Boolean = {
      //top
      if (preLayer.length == 1)
        return true

      //form a Layer , go up
      if (curLayer.length + 1 == preLayer.length)
      //be careful , if curLayer.length == 1 , should return true explicitly
      //not a just a boolean value true (don't get disturbed by Scala habit!!!)
      return dfs(curLayer, "")

      //for possible currentLayer
      //curLayer  0 ?
      //preLayer 0 1 2
      val i = curLayer.length
      val row = preLayer(i) - 'A'
      val column = preLayer(i + 1) - 'A'
      for (v <- allowedRelation(row)(column)) {
        if (dfs(preLayer, curLayer + v))
          return true
      }

      false
    }

    dfs(bottom, "")
  }

  //79. Word Search
  def exist(board: Array[Array[Char]], word: String): Boolean = {
    if (word == null || word.length < 1)
      return false

    val rows = board.length
    val columns = board(0).length
    val visited = Array.ofDim[Boolean](rows, columns)

    def dfs(x: Int, y: Int, idxOfWord: Int): Boolean = {
      if (idxOfWord == word.length - 1)
        return true

      val dx = Array(-1, 0, 1, 0)
      val dy = Array(0, 1, 0, -1)

      //record status of current path
      visited(x)(y) = true

      for (i <- 0 to 3) {
        val nextX = x + dx(i)
        val nextY = y + dy(i)
        if (nextX >= 0 && nextX < rows
          && nextY >= 0 && nextY < columns
          && !visited(nextX)(nextY)
          && board(nextX)(nextY) == word(idxOfWord + 1)) {

          if (dfs(nextX, nextY, idxOfWord + 1))
            return true
        }
      }

      // should reset current status for other paths
      visited(x)(y) = false

      false
    }

    for (i <- 0 until rows) {
      for (j <- 0 until columns) {
        //find pos of first char of word
        if (board(i)(j) == word(0)) {
          if (dfs(i, j, 0))
            return true
        }
      }
    }

    false
  }

  //464. Can I Win
  /*
    In the "100 game," two players take turns adding, to a running total, any integer from 1..10. The player who first causes the running total to reach or exceed 100 wins.

    What if we change the game so that players cannot re-use integers?

    For example, two players might take turns drawing from a common pool of numbers of 1..15 without replacement until they reach a total >= 100.

    Given an integer maxChoosableInteger and another integer desiredTotal, determine if the first player to move can force a win, assuming both players play optimally.

    You can always assume that maxChoosableInteger will not be larger than 20 and desiredTotal will not be larger than 300.

    Example

    Input:
    maxChoosableInteger = 10
    desiredTotal = 11

    Output:
    false

    Explanation:
    No matter which integer the first player choose, the first player will lose.
    The first player can choose an integer from 1 up to 10.
    If the first player choose 1, the second player can only choose integers from 2 up to 10.
    The second player will win by choosing 10 and get a total = 11, which is >= desiredTotal.
    Same with other integers chosen by the first player, the second player will always win.

    Game theory
    Be concise , for current status of first player(in first player's turn),
    if a choice leads to final victory
    or
    all the other choices except first player's can cause second player's failure,
    Return true.

    A binary number can represent current status of choices
    that have already been made by both players,
    and the accumulation of current choices.

    Considering for avoiding a path goes back to feasible status that
    have been visited,
    feasible status should be recorded.

    current desire = desiredTotal - accumulation
    feasibleOrNot = current desire -> Map[Int,Boolean]


  * */
  def canIWin(maxChoosableInteger: Int, desiredTotal: Int): Boolean = {
    //absolute winning
    if(desiredTotal <= 1)
      return true
    //never approach
    if(maxChoosableInteger * (1 + maxChoosableInteger) < desiredTotal * 2)
      return false

    val feasibleOrNot = Array.fill(desiredTotal + 1)(
      scala.collection.mutable.Map[Int,Boolean](0 -> false)
    )

    def dfs(status: Int, desire: Int):Boolean = {
      //status has been validated before
      if(feasibleOrNot(desire).getOrElse(status,false))
        return feasibleOrNot(desire)(status)

      //choose a number
      for(i <- maxChoosableInteger - 1 to 0 by -1){
        val curChoice = 1 << i
        //current number can be chosen
        if((status & curChoice) == 0){
          //reaching desire
          //or all the choices of "next turn"(player 2) lead to failure
          if(i + 1 >= desire || !dfs(status | curChoice, desire - i - 1)) {
            //record status for current dfs path
            feasibleOrNot(desire) += (status -> true)
            return true
          }
        }
      }

      //current path couldn't reach desire
      feasibleOrNot(desire) += (status -> false)

      false
    }

    dfs(0, desiredTotal)
  }



}
