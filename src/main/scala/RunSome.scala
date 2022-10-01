object RunSome {

  def timer[A](blockOfCode: => A) = {
    val startTime = System.nanoTime //系统纳米时间
    val result = blockOfCode
    val endTime = System.nanoTime
    val delta = endTime - startTime
    (result, delta / 1000000d)
  }

  def main(args: Array[String]): Unit = {
    //    val r = DFS_BFS.ladderLength("hit","cog",List("hot","dot","dog","lot","log","cog"))
    //
    //    println(r)

    //    val t = timer({
    //      val signArr = Array(1,1,1,1,0,0,0,0,1,1,1)
    //
    //      val arr1 = Array(0) ++ signArr :+ 0
    //      val arr2 = arr1.indices.zip(arr1).toMap.filter(_._2 == 0).toArray.map(_._1).sorted
    //      val arr3 = arr2.sliding(2).filter(x => x(1) - x(0) > 3).map(x => (x(0), x(1)))
    //
    //      println(arr3.mkString(","))
    //    })
    //
    //
    //    val time = t._2
    //    println(time)


    val r = ((1 << 5) - 1) & 0
    print(r)
    println("")
    println("Hello Scala")

  }

}
