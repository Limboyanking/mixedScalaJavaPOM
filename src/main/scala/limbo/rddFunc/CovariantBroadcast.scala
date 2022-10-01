package limbo.rddFunc

import org.apache.spark.broadcast.Broadcast

import scala.reflect.ClassTag


// ClassTag获取运行时类型， 用T协变
class CovariantBroadcast[+T:ClassTag](broadcast: Broadcast[T]) extends Serializable{

  val value:T = broadcast.value


}
