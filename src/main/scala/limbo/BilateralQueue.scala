package limbo


import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{::, Iterable, Nil, Queue}
import scala.collection.mutable.ListBuffer

object BilateralQueue{

  implicit def queue2BilateralQueue[T](q:Queue[T]) = new BilateralQueue[T](q)



}


/**
  scala的反射,scala的反射分为两个范畴运行时和编译时

  这两者之间的区别在于Environment,而Environment又是由universe决定的.反射的另一个重要的部分就是一个实体集合,
  而这个实体集合被称为mirror,有了这个实体集合我们就可以实现对需要反射的类进行对应的操作,如属性的获取,属性值得设置,以及对反射类方法的调用(其实就是成员函数的入口地址)!

  val cu = scala.reflect.macros.Universe

  val ru = scala.reflect.runtime.universe

  说的直白一些就是要获取可以操作类方法或者属性的mirror,
  而mirror又是Environment得来的,而Environment又是Universes得来的,而Universes根据运行时和编译时又可以分为两个领域

  -----------------------

  //                                <---- 1 2 3 4
  //               queue_out   queue_in
  //               head         last
  val queue = BilateralQueue(1,2,3,4)

  queue.append(0) // 0 1 2 3 4

//      val y = queue.+:(x) //0.029 ms
//      val y = queue.append(x) // 1645.4818 ms

//      val y = queue.dequeueFromIn //1761.1655 ms

//      val y = queue.enqueue(1 to 10000000).last //15623.3467 ms
//      val y = queue.enqueue(1 to 10000000).getLast //13367.1365 ms

//      val y = queue :+ x  //0.0319 ms
//      val y = queue.enqueue(5) // 0.0198 ms

  */
class BilateralQueue[T](queue:Queue[T]){

  val ru = scala.reflect.runtime.universe
  // 获取任意的TypeTag
  def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]

  val theType = ru.typeOf[queue.type]
  val mirror = ru.runtimeMirror(getClass.getClassLoader)

  val classQueue = theType.typeSymbol.asClass

  val instanceMirror = mirror.reflect(queue)
  val classMirror = mirror.reflectClass(classQueue)

  val ctorSymb = theType.decl(ru.termNames.CONSTRUCTOR).asMethod
  val ctorMethod = classMirror.reflectConstructor(ctorSymb)

  val inListTermSymb = theType.decl(ru.TermName("in")).asTerm.accessed.asTerm
  val outListTermSymb = theType.decl(ru.TermName("out")).asTerm.accessed.asTerm

  val inList = instanceMirror.reflectField(inListTermSymb).get.asInstanceOf[List[T]]
  val outList = instanceMirror.reflectField(outListTermSymb).get.asInstanceOf[List[T]]

  @deprecated("please use Queue.+:")
  def append[U >: T](x: U): Queue[U] = {

    ctorMethod(inList,x :: outList.asInstanceOf[List[U]]).asInstanceOf[Queue[U]]
  }

  def append[U >: T](iter: Iterable[U]): Queue[U] = {
    var i = iter.toList
    var res = queue.asInstanceOf[Queue[U]]
    while(i.nonEmpty){
      res = res.+:(i.head)
      i = i.tail
    }

    res
  }

  //Queue(1,2,3,4).dequeueFromIn ===> (4,Queue(1,2,3))
  def dequeueFromIn:(T,Queue[T]) =
    inList match {
      case Nil if outList.nonEmpty =>
        val buf = ListBuffer[T]()
        var out = outList
        while(out.tail.nonEmpty){
          buf += out.head
          out = out.tail
        }
        (out.head,ctorMethod(inList,buf.toList).asInstanceOf[Queue[T]])
      case x :: xs            => (x, ctorMethod(xs, outList).asInstanceOf[Queue[T]])
      case _                  => throw new NoSuchElementException("dequeue on empty queue")
    }

  def dequeueFromInOption:Option[(T,Queue[T])] = if(queue.isEmpty) None else Some(dequeueFromIn)

  @deprecated("only takes advantage after 100,000,00 times enqueue operation")
  def getLast:T =
    if (inList.nonEmpty) inList.head
    else if (outList.nonEmpty) outList.last
    else throw new NoSuchElementException("get last on empty queue")

  @deprecated("only takes advantage after 100,000,00 times enqueue operation")
  def getLastOption = if(queue.isEmpty) None else Some(getLast)

}
