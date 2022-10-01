package limbo

trait TraitTest{

  val x = (a:Int) => Math.asin(a)

  def sayHi

  def sayHello= println("Hello World")

}

abstract class Lord{

  def create = println("Create the world")

  def say

}

class Limbo extends Lord{

  val y = 0

  def say = println("I would rather keep silence")

  def sayHi = println("Hi")

}


object DynamicInject {

  def main(args: Array[String]): Unit = {

    val justSay = new Limbo with TraitTest {
      override def sayHi: Unit = println("Hi")
    }

    justSay.x(justSay.y)

    justSay.sayHello

  }

}