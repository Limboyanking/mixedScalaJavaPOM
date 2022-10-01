package limbo.rddFunc

import limbo.rddFunc.PrintRDD.ForString
import org.apache.spark.rdd.RDD

object PrintRDD{

  case class ForString[T](arr:Array[T]){

    override def toString: String = arr.mkString("|")

  }

  def arr2String[T](arr:Array[T]) = new ForString[T](arr)

  implicit def forRddFunc[T](rdd:RDD[_]) = new RddFunc[T](rdd)

  case class RddFunc[T](rdd:RDD[_]){
    def take4Print(n:Int) = {
      rdd.take(n)
        .map{
          case x if x.isInstanceOf[(T, T)] =>
            val t = x.asInstanceOf[(T, T)]
            val t1 = t._1.asInstanceOf[String]
            val t11 = t._1.asInstanceOf[Array[T]]
            val t2 = t._2.asInstanceOf[Array[T]]
              .map(_.toString).asInstanceOf[Array[T]]

            if(t._1.getClass == classOf[String])
              (t1,arr2String(t2))
            else
              (arr2String(t11),arr2String(t2))
          case y if y.isInstanceOf[Array[T]] =>
            val a = y.asInstanceOf[Array[T]]
            arr2String(a)
          case _ =>
        }
        .foreach(x => println(x + "\n"))
    }
  }




}

trait PrintRDD {

//  def printRddWithCount[T](rdd: RDD[_], rddName: String, columns: Array[T])(implicit arr2String:Array[T] => ForString[T]): Unit = {
//    val count = rdd.treeAggregate(0)((a,b) => a + 1,(x,y) => x + y)
//    print("\n")
//    println("rddName: " + rddName)
//    println("rddCount: " + count)
//    println("columns: " + arr2String(columns))
//    rdd.take(5)
//      .map {
//        case x if x.isInstanceOf[(T, T)] =>
//          val t = x.asInstanceOf[(T, T)]
//          val t1 = t._1.asInstanceOf[String]
//          val t11 = t._1.asInstanceOf[Array[T]]
//          val t2 = t._2.asInstanceOf[Array[T]]
//            .map(_.toString).asInstanceOf[Array[T]]
//
//          if(t._1.getClass == classOf[String])
//            (t1,arr2String(t2))
//          else
//            (arr2String(t11),arr2String(t2))
//        case y if y.isInstanceOf[Array[T]] =>
//          val a = y.asInstanceOf[Array[T]]
//          arr2String(a)
//        case _ =>
//      }
//      .foreach(println)
//    print("\n")
//  }
//

  def printRddWithCount[T](rdd: RDD[_], rddName: String, columns: Array[T]): Unit = {
    val count = rdd.treeAggregate(0)((a,b) => a + 1,(x,y) => x + y)
    print("\n")
    println("rddName: " + rddName)
    println("rddCount: " + count)
    println("columns: " + ForString(columns))

    import PrintRDD.forRddFunc
    rdd.take4Print(5)

    print("\n")
  }


  def printRddWithCount[T](rdd: RDD[_], rddName: String): Unit = {
    val count = rdd.treeAggregate(0)((a,b) => a + 1,(x,y) => x + y)
    print("\n")
    println("rddName: " + rddName)
    println("rddCount: " + count)

    import PrintRDD.forRddFunc
    rdd.take4Print(5)

    print("\n")
  }

  def printRdd[T](rdd: RDD[_], rddName: String, columns: Array[T]): Unit = {
    print("\n")
    println("rddName: " + rddName)
    println("columns: " + ForString(columns))

    import PrintRDD.forRddFunc
    rdd.take4Print(5)

    print("\n")
  }

  def printRdd[T](rdd: RDD[_], rddName: String): Unit = {
    print("\n")
    println("rddName: " + rddName)

    import PrintRDD.forRddFunc
    rdd.take4Print(5)

    print("\n")
  }


}
