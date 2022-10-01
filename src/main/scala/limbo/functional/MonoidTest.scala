package limbo.functional

object MonoidTest {

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a: Option[A], b: Option[A]): Option[A] = a orElse b

    def zero = None
  }

  def main(args: Array[String]): Unit = {
    val v = Vector("a", "rose", "is", "a", "rose")
    val resMap = MonoidTool.bag(v)
    println(resMap)

    val testList = List(1, 2, 3, 4)
    val sumAndCountMonoid = MonoidTool.productMonoid(MonoidTool.intAddition, MonoidTool.intAddition)

    val ret = ListFoldable.foldMap(testList)(x => (1, x))(sumAndCountMonoid)

    println(ret)

    val intAddtion = new Monoid[Int] {
      def op(a1: Int, a2: Int) = a1 + a2

      def zero = 0
    }

  }


}
