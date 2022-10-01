package limbo.rddFunc

import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession

import scala.reflect.ClassTag
import scala.reflect.api.JavaUniverse
import limbo.rddFunc.RddFunctions.{convert2PairRddFunc,convert2CovariantBroadcast,convert2RDDFunc}

object ExtraRDDFunction {

  val sparkUtils = SparkSession.getActiveSession.get

}

class ExtraRDDFunction[T: ClassTag](rdd: RDD[T]) {

  lazy val ru: JavaUniverse = scala.reflect.runtime.universe

  val runtimeClass: Class[_] = ru.RuntimeClassTag.runtimeClass
  val classTagT: ClassTag[T] = ClassTag[T](runtimeClass)
  val classTagArrayT: ClassTag[Array[T]] = ClassTag[Array[T]](runtimeClass).asInstanceOf[ClassTag[Array[T]]]

  def broadcast: Broadcast[Array[T]] = ExtraRDDFunction.sparkUtils.sparkContext.broadcast(rdd.treeAggregate(Array[T]())((a, b) => a :+ b, (x, y) => x ++ y))

  /**
    * RDD(parNum,NumOfCurrentPartition)
    */
  def getNumOfEachPartition: RDD[(Int, Int)] = rdd.mapPartitionsWithIndex((par, iter) => Iterator((par, iter.length)))

  def getSkewnessInBetweenPartitions: Double = {
    val arr = rdd.getNumOfEachPartition.map(_._2).collect()
    val skewness = ExtraPairRDDFunction.getSkewness(arr)

    skewness
  }

  def take4Print(n: Int): Unit = {
    rdd.take(n)
      .map {
        case y if y.isInstanceOf[Iterable[_]] =>
          y.asInstanceOf[Iterable[_]].toList
      }
      .foreach(x => println(x + "\n"))
  }


}
