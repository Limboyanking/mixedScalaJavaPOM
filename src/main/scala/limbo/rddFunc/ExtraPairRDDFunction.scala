package limbo.rddFunc

import limbo.rddFunc.RddFunctions.convert2PairRddFunc
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd._
import org.apache.spark.sql.SparkSession

import scala.collection.parallel.CollectionConverters.IterableIsParallelizable
import scala.reflect.ClassTag
import scala.util.Random

object ExtraPairRDDFunction {

  val sparkUtils: SparkSession = SparkSession.getActiveSession.get

  def getSkewness(arr: Array[Int]): Double = {
    require(arr.nonEmpty && arr.length >= 2, "array length must be greater than or equal to 2")

    val len = arr.length
    val mean = arr.sum / len //均值

    def absMinus(mean: Double)(x: Double) = Math.abs(x - mean)

    val minus = absMinus(mean) _
    val variance = arr.map(x => Math.pow(minus(x), 2)).sum / len //方差
    lazy val devition = Math.pow(variance, 0.5) //标准差

    val skewness = if (devition == 0) 0 else {
      val ex3 = arr.map(Math.pow(_, 3)).sum / len
      val skew = (ex3 - 3 * mean * variance - Math.pow(mean, 3)) / Math.pow(devition, 3)

      Math.abs(skew)
    }

    skewness
  }

}

/*

 def treeAggregate[U: ClassTag](zeroValue: U)(
     seqOp: (U, T) => U,
     combOp: (U, U) => U,
     depth: Int = 2): U = withScope {
   require(depth >= 1, s"Depth must be greater than or equal to 1 but got $depth.")
   if (partitions.length == 0) {
     Utils.clone(zeroValue, context.env.closureSerializer.newInstance())
   } else {
     val cleanSeqOp = context.clean(seqOp)
     val cleanCombOp = context.clean(combOp)
     val aggregatePartition =
       (it: Iterator[T]) => it.aggregate(zeroValue)(cleanSeqOp, cleanCombOp)
     var partiallyAggregated = mapPartitions(it => Iterator(aggregatePartition(it)))
     var numPartitions = partiallyAggregated.partitions.length

     val scale = math.max(math.ceil(math.pow(numPartitions, 1.0 / depth)).toInt, 2)
     /*
       scale = numPartitions^(1/depth)
       ex:
         1000^(1/3) = 10  scale = 10

       1000 > 10 + math.ceil(1000 / 10)

       curNumPartitions = 1000 / 10 = 100

       i % 100         i % 10
       0 < i < 999 => 0 < i < 99 => 0 < i < 9

       reduceByKey 2 times , by the way shrink the numPartitions

     */

     // If creating an extra level doesn't help reduce
     // the wall-clock time, we stop tree aggregation.

     // Don't trigger TreeAggregation when it doesn't save wall-clock time
     while (numPartitions > scale + math.ceil(numPartitions.toDouble / scale)) {
       numPartitions /= scale
       val curNumPartitions = numPartitions
       partiallyAggregated = partiallyAggregated.mapPartitionsWithIndex {
         (i, iter) => iter.map((i % curNumPartitions, _))
       }.reduceByKey(new HashPartitioner(curNumPartitions), cleanCombOp).values
     }
     partiallyAggregated.reduce(cleanCombOp)
   }
 }

 */

/**
  * StackOverFlow Joe K Aug 10'17 at 18:38:
  * This comes from using a pair rdd function generically.
  * The reduceByKey method is actually a method of the PairRDDFunctions class,
  * which has an implicit conversion from RDD:
  * *
  * implicit def rddToPairRDDFunctions[K, V](rdd: RDD[(K, V)])
  * (implicit kt: ClassTag[K], vt: ClassTag[V], ord: Ordering[K] = null): PairRDDFunctions[K, V]
  * *
  * So it requires several implicit type classes.
  * Normally when working with simple concrete types, those are already in scope.
  * But you should be able to amend your method to also require those same implicits:
  * *
  * class ExtraPairRDDFunction[K:ClassTag:Ordering, V:ClassTag](rdd: RDD[(K, V)])
  *
  */
class ExtraPairRDDFunction[K, V](rdd: RDD[(K, V)])(implicit kt: ClassTag[K], vt: ClassTag[V], ord: Ordering[K] = null)
  extends Serializable {

  type T = (K, V)

  def groupByKeyViaAggregate(): RDD[(K, Array[V])] = rdd.aggregateByKey(Array[V]())((a, b) => a :+ b, (x, y) => x ++ y)

  /**
    * @param prefixRange 0 ~  prefixRange - 1 random number added to the original key
    * @note please be ware of OOM error
    **/
  def aggregateByKeyVia2Stages[U: ClassTag](zeroValue: U)(seqOp: (U, V) => U,
                                                          combOp: (U, U) => U,
                                                          whether2JudgeSkewness: Boolean = false,
                                                          skewnessThreshold: Double = 2,
                                                          prefixRange: Int = 10): RDD[(String, U)] = {
    require(skewnessThreshold >= 0, "SkewnessThreshold must be greater than or equal to 0")
    require(!keyClass.isArray, "Cannot use array keys")

    lazy val skewness = {
      rdd.cache()
      rdd.getSkewnessInBetweenPartitions
    }
    // do mean to judge skewness and not skew enough
    if (whether2JudgeSkewness && skewness <= skewnessThreshold) {
      println("Current RDD skewness is: " + skewness)
      rdd.aggregateByKey(zeroValue)(seqOp, combOp).map(x => (x._1.toString, x._2))
    }
    // skew enough or do not mean to judge skewness
    else {
      val randomPrefixRdd = rdd.map(x => {
        val random = new Random()
        val prefix = random.nextInt(prefixRange).toString
        val k = prefix + "_" + x._1.toString

        (k, x._2)
      })

      val firstStage = randomPrefixRdd.aggregateByKey(zeroValue)(seqOp, combOp)

      val removedRandomPrefixRdd = firstStage.map(x => {
        val origin = x._1.substring(2)
        (origin, x._2)
      })

      val secondStage = removedRandomPrefixRdd.reduceByKey(combOp)

      secondStage
    }
  }

  /**
    * @param prefixRange 0 ~  prefixRange - 1 random number added to the original key
    * @note please be ware of OOM error
    **/
  def reduceByKeyVia2Stages(func: (V, V) => V,
                            whether2JudgeSkewness: Boolean = false,
                            skewnessThreshold: Double = 2,
                            prefixRange: Int = 10): RDD[(String, V)] = {
    require(skewnessThreshold >= 0, "SkewnessThreshold must be greater than or equal to 0")
    require(!keyClass.isArray, "Cannot use array keys")

    lazy val skewness = {
      rdd.cache()
      rdd.getSkewnessInBetweenPartitions
    }
    // do mean to judge skewness and not skew enough
    if (whether2JudgeSkewness && skewness <= skewnessThreshold) {
      println("Current RDD skewness is: " + skewness)
      rdd.reduceByKey(func).map(x => (x._1.toString, x._2))
    }
    // skew enough or do not mean to judge skewness
    else {
      val randomPrefixRdd = rdd.map(x => {
        val random = new Random()
        val prefix = random.nextInt(prefixRange).toString
        val k = prefix + "_" + x._1.toString

        (k, x._2)
      })

      val firstStage = randomPrefixRdd.reduceByKey(func)

      val removedRandomPrefixRdd = firstStage.map(x => {
        val origin = x._1.substring(2)
        (origin, x._2)
      })

      val secondStage = removedRandomPrefixRdd.reduceByKey(func)

      secondStage
    }
  }

  def broadCast: Broadcast[Array[(K, V)]] = ExtraPairRDDFunction.sparkUtils.sparkContext.broadcast(
    rdd.treeAggregate(Array[(K, V)]())(
      (a, b) => a :+ b
      ,
      (x, y) => x ++ y
    ))

  def toMapThenBroadcast: Broadcast[Map[K, V]] = ExtraPairRDDFunction.sparkUtils.sparkContext.broadcast(
    rdd.treeAggregate(Map[K, V]())(
      (a, b) => a + (b._1 -> b._2)
      ,
      (x, y) => x ++ y
    ))

  /**
    * @param bro the Broadcast that needs to be joined( be implicit converted)
    * @note the Broadcast.value.head must be Key-Value structure
    **/
  def joinBroadcast[W: ClassTag](bro: CovariantBroadcast[Iterable[(K, W)]],
                                 isKeyDuplicated: Boolean = false): RDD[(K, (V, W))] = {
    require(!keyClass.isArray, "Cannot use array keys")
    if (isKeyDuplicated)
      rdd.flatMap(x => {
        val k = x._1
        val v = x._2
        val broV = bro.value match {
          case b: Iterable[(K, W)] if b.nonEmpty => Some(b)
          case c if c.isEmpty => None
        }
        //并发容器
        val concurrentQueue = new java.util.concurrent.ConcurrentLinkedDeque[W]()
        val r = for {
          iterable <- broV
          res <- { //iterable 匹配可能失败，需要Option处理
            iterable.par.foreach(y => { //改善匹配效率
              val broK = y._1
              val w = y._2
              if (k == broK)
                concurrentQueue.add(w)
            })
            import scala.collection.JavaConverters._
            if (concurrentQueue.isEmpty)
              None
            else
              Some(concurrentQueue.asScala.map(w => (k, (v, w))))
          }
        } yield res

        r.getOrElse(List())
      })
    else {
      rdd.map(x => {
        val broV = bro.value match {
          case a: Map[K, W] if a.nonEmpty => Some(a)
          case b: Iterable[(K, W)] if b.nonEmpty => Some(b.toMap)
          case c if c.isEmpty => None
        }
        val k = x._1
        val v = x._2
        val r = for {
          mapping <- broV
          w <- mapping.get(k)
        } yield (k, (v, w))

        r
      })
        .filter(_.isDefined)
        .map(_.get)
    }
  }

  def leftOuterJoinBroadcast[W: ClassTag](bro: CovariantBroadcast[Iterable[(K, W)]],
                                          isKeyDuplicated: Boolean = false): RDD[(K, (V, Option[W]))] = {
    require(!keyClass.isArray, "Cannot use array keys")
    if (isKeyDuplicated)
      rdd.flatMap(x => {
        val k = x._1
        val v = x._2
        val broV = bro.value match {
          case b: Iterable[(K, W)] if b.nonEmpty => Some(b)
          case c if c.isEmpty => None
        }
        //并发容器
        val concurrentQueue = new java.util.concurrent.ConcurrentLinkedDeque[W]()
        val r = broV.map(iterable => {
          iterable.par.foreach(y => { //改善匹配效率
            val broK = y._1
            val w = y._2
            if (k == broK)
              concurrentQueue.add(w)
          })
          import scala.collection.JavaConverters._
          if (concurrentQueue.isEmpty)
            List((k,(v,None)))
          else
            concurrentQueue.asScala.map(w => (k, (v, Some(w))))
        })

        r.getOrElse(List())
      })
    else {
      rdd.map(x => {
        val broV = bro.value match {
          case a: Map[K, W] if a.nonEmpty => Some(a)
          case b: Iterable[(K, W)] if b.nonEmpty => Some(b.toMap)
          case c if c.isEmpty => None
        }
        val k = x._1
        val v = x._2
        val r = for {
          mapping <- broV
        } yield  (k,(v,mapping.get(k)))

        r
      })
        .filter(_.isDefined)
        .map(_.get)
    }
  }

  /**
    * In case of skewRdd has relatively small amount keys that duplicated a lot of K-V
    * 倾斜的RDD中有少量key 包含较多的 key-value
    *
    * @param steadyRdd      rdd in relatively uniform distribution
    * @param sampleFraction sample fraction of the skewRdd
    * @param topRange       split from the rank of the number of occurrences of keys top down to topRange
    * @note please be ware of OOM error
    **/
  def sampleTakeThenJoin[W: ClassTag](steadyRdd: RDD[(K, W)],
                                      sampleFraction: Double = 0.1,
                                      topRange: Int = 100): RDD[(String, (V, W))] = {
    require(!keyClass.isArray, "Cannot use array keys")
    require(sampleFraction > 0 && topRange > 0, "both sampleFraction topRange should be greater than 0")

    rdd.cache()
    steadyRdd.cache()

    val skewedKeys = rdd.sample(false, sampleFraction)
      .getNumOfEachKey
      .reverseKeyValue
      .sortByKey(false)
      .take(topRange)
      .map(_._2)
      .toSet

    //加前缀
    val skewedKeysRdd = rdd.filter(x => skewedKeys.contains(x._1))
      .map(x => {
        val random = new Random()
        val prefix = random.nextInt(topRange).toString
        val k = prefix + "_" + x._1.toString

        (k, x._2)
      })
    val skewResidualRdd = rdd.filter(x => !skewedKeys.contains(x._1))

    // 膨胀出前缀对应 skewedKeysRdd
    val stableCorrespondingRdd = steadyRdd.filter(x => skewedKeys.contains(x._1))
      .flatMap(x => {
        val arr = Stream.range(0, topRange).map(_.toString + "_")
        val kv = Stream.fill(topRange)(x)
        val z = arr.zip(kv).map(y => {
          val k = y._1 + y._2._1.toString
          (k, y._2._2)
        })

        z
      })
    val stableResidualRdd = steadyRdd.filter(x => !skewedKeys.contains(x._1))

    val joinPart1 = skewedKeysRdd.join(stableCorrespondingRdd)
      .map(x => {
        val origin = x._1.substring(2)
        (origin, x._2)
      })
    val joinPart2 = skewResidualRdd.join(stableResidualRdd).map(x => (x._1.toString, x._2))

    joinPart1.union(joinPart2)
  }

  /**
    * In case of skewRdd has large amount of keys that duplicated a lot of K-V
    * 倾斜的RDD中有众多的key 包含较多的 key-value
    *
    * @param steadyRdd   rdd in relatively uniform distribution
    * @param prefixRange 0 ~  prefixRange - 1 random number added to the original key
    *
    **/
  def prefixJoin[W: ClassTag](steadyRdd: RDD[(K, W)],
                              prefixRange: Int = 10): RDD[(String, (V, W))] = {
    require(!keyClass.isArray, "Cannot use array keys")
    require(prefixRange > 0, "prefixRange should be greater than 0")

    val rddWithPrefix = rdd.map(x => {
      val random = new Random()
      val prefix = random.nextInt(prefixRange).toString
      val k = prefix + "_" + x._1.toString

      (k, x._2)
    })

    val steadyCorrespondRdd = steadyRdd.flatMap(x => {
      val arr = Stream.range(0, prefixRange).map(_.toString + "_")
      val kv = Stream.fill(prefixRange)(x)
      val z = arr.zip(kv).map(y => {
        val k = y._1 + y._2._1.toString
        (k, y._2._2)
      })

      z
    })

    val rddWithoutPrefix = rddWithPrefix.join(steadyCorrespondRdd)
      .map(x => {
        val origin = x._1.substring(2)
        (origin, x._2)
      })

    rddWithoutPrefix
  }

  //  def mapWithBroadcast[_:ClassTag,R](f: (T,_ *) => R,bro:CovariantBroadcast[_] *) = rdd.map(x => f(x,bro.map(_.value):_*))

  //  def foreachWithBroadcast[_:ClassTag,R](f: (T,_ *) => R,bro:CovariantBroadcast[_] *) = rdd.foreach(x => f(x,bro.map(_.value):_*))

  def collectViaTreeAggregate(depth: Int = 2): Array[(K, V)] = rdd.treeAggregate(Array[(K, V)]())(
    (a, b) => a :+ b
    ,
    (x, y) => x ++ y
    ,
    depth)

  def collectAsMapViaTreeAggregate(depth: Int = 2): Map[K, V] = rdd.treeAggregate(Map[K, V]())(
    (a, b) => a ++ Map(b._1 -> b._2)
    ,
    (x, y) => x ++ y
    ,
    depth)

  /**
    * @note RDD(parNum,NumOfCurrentPartition)
    */
  def getNumOfEachPartition: RDD[(Int, Int)] = rdd.mapPartitionsWithIndex((par, iter) => Iterator((par, iter.length)))

  def getNumOfEachKey: RDD[(K, Long)] = rdd.map(x => (x._1, 1L)).reduceByKey(_ + _)

  def reverseKeyValue: RDD[(V, K)] = rdd.map(x => (x._2, x._1))

  def getSkewnessInBetweenPartitions: Double = {
    val arr = rdd.getNumOfEachPartition.map(_._2).collect()
    val skewness = ExtraPairRDDFunction.getSkewness(arr)

    skewness
  }

  def getSkewnessInBetweenKeys(sampleFraction: Double = 1): (Double, Array[(K, Int)]) = {
    require(sampleFraction > 0 && sampleFraction < 1, "sampleFraction must be in between (0,1)")
    val arr = if (sampleFraction == 1)
      rdd.getNumOfEachKey
        .reverseKeyValue
        .sortByKey(false)
        .collect()
    else
      rdd.sample(false, sampleFraction)
        .getNumOfEachKey
        .reverseKeyValue
        .sortByKey(false)
        .collect()

    val skewness = ExtraPairRDDFunction.getSkewness(arr.unzip._1.map(_.toInt))

    (skewness, arr.map(x => (x._2, x._1.toInt)))
  }

  def take4Print(n: Int): Unit = {
    rdd.take(n)
      .map {
        x =>
          val t1 = x._1.toString
          val t11 = x._1.asInstanceOf[Iterable[_]].map(_.toString).toList
          val t2 = x._2.asInstanceOf[V].toString
          val t22 = x._2.asInstanceOf[Iterable[_]].map(_.toString).toList

          if (t1.getClass == classOf[String] && t2.getClass != classOf[Iterable[String]])
            (t1, t2)
          else
            (t11, t22)
      }
      .foreach(x => println(x + "\n"))
  }

  private def keyClass: Class[_] = kt.runtimeClass

  private def valueClass: Class[_] = vt.runtimeClass

  private def keyOrdering: Option[Ordering[K]] = Option(ord)

}
