package limbo.multiThread

object Run {
  //    val m = Map("A" -> 2, "B"->1, "C"->1)
  //    val n = Map("D" -> 2, "E"->1,"B"->1)
  //    val x = Map("F" -> 2 , "G" -> 1,"B" -> 1)
  //    val exp = m ++ n ++ x
  //    println(exp)
  //
  //    val l = 1::2::Nil
  //    println(l)
  //
  //    val stringList = List[String]("20","test")
  //
  //    val ret = Error_Handling.traverse(stringList)(s => Error_Handling.Try(s.toInt))
  //
  //    println(ret)
  //
  //    val filt = Laziness.Stream(1,2,3,4).filter(x => x != 2)
  //
  //    println(filt.toList)
  //
  //    val roman = Solution.intToRoman(19499)
  //
  //    println(roman)

  //
  //                   sorted: List(-5, -4, 1, 1, 1, 1, 2) target: -4
  //    val t = Solution.kSumSolution(Array(1,-5,1,-4,2,1),6,-4)
  //    println(t)

  //    val javaSolution = new JavaSolution
  //    val javaTest = javaSolution.fourSum(Array(1, 0, -1, 0, -2, 2),0)
  //    println(javaTest)
  //
  //    val t = Solution.generateParenthesis(4)
  //    println(t)

  //    val ns:Rand[List[Int]] =
  //      RNG.int.flatMap(x =>
  //        RNG.int.flatMap(y =>
  //          RNG._ints(x).map(xs =>
  //            xs.map(_ % y)
  //          )
  //        )
  //      )


  //    val input = List[States.Input](Coin,Coin,Coin,Coin)
  //
  //    val r = States.Candy.simulateMachine2(input,new Machine(true,5,10))
  //
  //    println(r)

  def main(args: Array[String]): Unit = {
    import java.util.concurrent.Executors
    val es = Executors.newFixedThreadPool(4)

    println("currentThread: " + Thread.currentThread().getName)
    val echoer = Actor[String](es) {
      msg => println("currentThread: " + Thread.currentThread().getName + " " + s"Got message: '$msg'")
    }
    println(Thread.currentThread().getName + " not being blocked")

    echoer ! "hello"

    echoer ! "Good bye"


  }

}
