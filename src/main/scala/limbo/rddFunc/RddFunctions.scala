package limbo.rddFunc

import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.RDD

import scala.reflect.ClassTag

object RddFunctions {

  lazy val ru = scala.reflect.runtime.universe
  val mirror = ru.runtimeMirror(getClass.getClassLoader)
  val runtimeClass = ru.RuntimeClassTag.runtimeClass

  // 获取任意的TypeTag
  def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]

  implicit def convert2PairRddFunc[K: ClassTag : Ordering, V:ClassTag](rdd: RDD[(K, V)]) = new ExtraPairRDDFunction[K, V](rdd)

  implicit def convert2RDDFunc[T:ClassTag](rdd: RDD[T]) = new ExtraRDDFunction(rdd)

  implicit def convert2CovariantBroadcast[T:ClassTag](bro: Broadcast[T]) = new CovariantBroadcast(bro)


}

class RddFunctions {



}
