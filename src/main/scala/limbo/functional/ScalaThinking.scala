package limbo.functional

import limbo.functional.Parallelism.Nonblocking.Par
import limbo.functional.States.RNG.SimpleRNG
import limbo.functional.States.{RNG, State}
import limbo.functional.Testing.Prop._
import limbo.multiThread.Actor

import java.util.concurrent._

object Getting_Started {
  def fib(n: Int): Int = {
    def go(count: Int = 3, n1: Int, n2: Int): Int = {
      if (count >= n)
        n1 + n2
      else go(count + 1, n2, n1 + n2)
    }

    go(3, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n == as.length - 1)
        true
      else if (ordered(as(n), as(n + 1)))
        loop(n + 1)
      else false
    }

    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) =>
      (b: B) => f(a, b)

  // f(a,b) 会得到类型为C的量

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  //    val arr:Array[Int] = Array(1,2,3,2)
  //    val order = (a:Int,b:Int)=> a < b

  //  val order = new Function2[Int,Int,Boolean]{
  //        def apply(a:Int , b:Int) = a < b
  //      }
  //      order 是一段创建函数对象的语法糖
  //      order.apply(1,2) 等价于 order(1,2)

  //    println(order.apply(1,2) && order(1,2))
  //    println(isSorted(arr,order))

  // 先执行f 后执行 math.sin  x => Sin(pi / 2 - x) = Cos(x)
  val f = (x: Double) => math.Pi / 2 - x

  val cos = f andThen math.sin

  //  Cons在标准库中的版本写为:: 右结合
  //	1::2::Nil 等于 1::(2::Nil) 等于	List(1,2)
  val l = 1 :: 2 :: Nil

}

object Error_Handling {
  /** errorHandling */
  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = {
    for {
      aa <- a
      bb <- b
      cc <- c
    } yield f(aa, bb, cc)
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    /**
      * 编译器会对例如 aa <- a 这样的绑定(binding)翻译成 flatMap
      * 对最后一个绑定 bb <- b 和 yield ，视为 map 操作
      */
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

    /**
      * 等价于
      * 目标：将 f:(A,B)=>C 提升为 f:(Option[A],Option[B]) => Option[C]
      * 思路：
      * (1) a:Option[A].flatMap(A=>Option[C]) 得到Option[C] ,
      * 因为要保证 A => C 的过程能够处理未定义的情况，选择flatMap, A 映射到 Option[C]
      * (2) b:Option[B].map(B=>C) 得到 Option[C]
      * (3) B=>C ???
      * (4) f  = (A,B) => C
      * (5) A flatMap ==> b:Option[B].map( f(A,B)=>C ) = Option[C]
      * *
      * a flatMap(            // a flatMap 意指 A => Option[C] 并将所有A映射的结果连成一片
      * aa => b map(        // aa 意指 A
      * bb => f(aa, bb)   // bb 意指 B , B => f(A,B) = C , Option[B] => Option[C]
      * )                   //  A => Option[C]
      * )
      */
  }

  val add3 = (a: Double, b: Double, c: Double) => a + b + c

  def liftedAdd3(a: String, b: String, c: String) = {
    val liftedA: Option[Double] = Option(a.toDouble)
    val liftedB: Option[Double] = Option(b.toDouble)
    val liftedC: Option[Double] = Option(c.toDouble)

    map3(liftedA, liftedB, liftedC)(add3)
  }

  val add = (a: Double, b: Double) => a + b

  def liftedAdd(a: String, b: String) = {
    val liftedA: Option[Double] = Option(a.toDouble)
    val liftedB: Option[Double] = Option(b.toDouble)
    map2(liftedA, liftedB)(add)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def parseInt(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

}

object Laziness {

  import Stream._

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  trait Stream[+A] {
    def toList: List[A] = {
      @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }

      go(this, List()).reverse
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
        case _ => z
      }

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) =>
        if (f(h)) cons(h, t)
        else t)

    /*
  The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.

  The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results,
  which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result.
  Here, we simply extract the accumulated list once finished.
  */
    //    def scanRight[A,B](z: B)(f: (A, => B) => B): Stream[B] =
    //      foldRight((z, Stream(z)))((a, p0) => {
    //        // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
    //        lazy val p1 = p0
    //        val b2 = f(a, p1._1)
    //        (b2, cons(b2, p1._2))
    //      })._2

  }

  object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

  }

  //输出指定数字的递归流
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  //输出一个等差数列
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  // 流构造函数
  //接收一个初始状态，
  //以及一个在生成Stream的过程中用于产生下一状态和下一个值的函数
  //只是利用状态S生成元素
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  //  def mapViaUnfold[A,B](f: A => B): Stream[B] =
  //    unfold(this) {
  //      case Cons(h,t) => Some((f(h()), t()))
  //      case _ => None
  //    }

}

object States {

  trait RNG {
    def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
  }

  object RNG {

    case class SimpleRNG(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
        val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
        val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
        (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      }
    }

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (if (i < 0) -(i + 1) else i, r)
    }

    // We generate an integer >= 0 and divide it by one higher than the
    // maximum. This is just one possible solution.
    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + 1), r)
    }

    //生成指定数量的随机整型数
    def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
      def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
        if (count == 0)
          (xs, r)
        else {
          val (x, r2) = r.nextInt
          go(count - 1, r2, x :: xs)
        }

      go(count, rng, List())
    }

    val int: Rand[Int] = _.nextInt

    val boolean:Rand[Boolean] = x => {
      val (i,r) = x.nextInt
      if(i > 0)
        (true,r)
      else
        (false,r)
    }

    type Rand[+A] = RNG => (A, RNG)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, r1) = ra(rng)
        val (b, r2) = rb(r1)
        (f(a, b), r2)
      }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] = both(int, double)

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

    def _ints(count: Int): Rand[List[Int]] =
      sequence(List.fill(count)(int))

    // 通过一个Rand[A] 来生成一个随机的A，然后使用这个A，生成一个Rand[B]
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, r1) = f(rng)
        g(a)(r1) // We pass the new state along
      }

    // 在 nonNegativeLessThan 里根据 nonNegativeInt生成的值选择是否进行重试
    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }
    }

    def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => unit(f(a)))

    /*
      flatMap(ra)(a => c:Rand[C])
      c = f(a,b)
      c:Rand[C] = map(rb)(b => f(a,b))

    */
    def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => map(rb)(b => f(a, b)))


  }

  import State._

  //flatMap可顺带传递状态，map只是改变值
  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }

  object State {
    type Rand[A] = State[RNG, A]

    def unit[S, A](a: A): State[S, A] =
      State(s => (a, s))

    // The idiomatic solution is expressed via foldRight
    def sequenceViaFoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] =
      sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

    // This implementation uses a loop internally and is the same recursion
    // pattern as a left fold. It is quite common with left folds to build
    // up a list in reverse order, then reverse it at the end.
    // (We could also use a collection.mutable.ListBuffer internally.)
    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
      def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
        actions match {
          case Nil => (acc.reverse, s)
          case h :: t => h.run(s) match {
            case (a, s2) => go(s2, t, a :: acc)
          }
        }

      State((s: S) => go(s, sas, List()))
    }

    def sequenceViaFunc[S, A](sas: List[State[S, A]], f: (A, S) => S): State[S, List[A]] = {
      def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
        actions match {
          case Nil => {
            println(s)
            (acc.reverse, s)
          }
          case h :: t => h.run(s) match {
            case (a, _) => go(f(a, s), t, a :: acc)
          }
        }

      State((s: S) => go(s, sas, List()))
    }

    //modify返回一个状态行为，用函数f修改输入状态，
    //它生成一个Unit，表示修改了状态但没有其它返回值
    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    def modify2[S](f: S => S): State[S, Unit] = get.flatMap(
      s => set(f(s))
    )

    //get行为简单地传入输入状态
    def get[S]: State[S, S] = State(s => (s, s))

    //set行为由一个新状态s构成。结果行为忽略输入状态，
    //替换成为了一个新状态并返回()
    //                                       S =>  Unit S
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  }


  /**
    * 糖果售货机
    * 规则：
    * 对一个锁定状态的售货机投入一枚硬币，如果有剩余的糖果它将变为非锁定状态
    * 对一个非锁定状态的售货机按下按钮，它将给出糖果并变回锁定状态
    * 对一个锁定状态的售货机按下按钮或对非锁定状态的售货机投入硬币，什么都不做
    * 售货机在“输出”糖果时忽略所有“输入”
    * *
    * 若当前售货机有10个硬币、5个糖果，成功地买了4个糖果，输出(14,1)
    *
    */
  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  // Machine 当成S状态看待
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Candy {

    // (input 旧状态) => 新状态
    def update = (i: Input) => (s: Machine) =>
      (i, s) match {
        case (_, Machine(_, 0, _)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)
      }

    def update2 = (i: Input, s: Machine) =>
      (i, s) match {
        case (_, Machine(_, 0, _)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)
      }

    // Machine 当成S状态看待                          S           A
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- sequence(inputs map (modify[Machine] _ compose update)) //先 update 后 modify
      s <- get
    } yield (s.coins, s.candies)

    def simulateMachine2(inputs: List[Input], m: Machine) =
      sequenceViaFunc(inputs map (x => State((m: Machine) => (x, m))), update2)


    /**
      * _ compose update  意指  update(i)(Machine)
      * *
      * Machine 当成S状态看待   S    Unit
      * modify 包装成 State[Machine,()]
      * *
      * inputs map 得到 List[ State[Machine,Unit] ]
      * *
      * sequence 按次序传递 State
      *
      *
      */

    val m: Machine = update(Coin)(Machine(false, 0, 0))
    //    val s = modify(_ => m)
    //    s

    //  s <- get // get: s => State(s,s) 此处 s 即为 Machine
    //}yield
    //等同于State.map(f:A => B):State()
    //此处因为get 的缘故 map 内传递的是 S(Machine)


  }


  /*
  type Rand[+A] = RNG => (A, RNG)
  def nonNegativeInt(nrg:NRG):(Int,NRG)  就是一个 Rand[Int]
  def rollDice = flatMap(nonNegativeInt)(Int => Rand[Int])

  case class  Simple extends NRG
  Simple(5):NRG

  nonNegative 生成一个非负整数，
  用这个数生成一个 Rand[Int] , 这个Rand内的 Int 大于零小于6
  */

  // rollDice(RNG):(RNG,Int)
  def rollDice: States.RNG.Rand[Int] = States.RNG.nonNegativeLessThan(6)

  // 用不同的种子试 Simple(5) 得0 , 可以保证每次传入 SimpleRNG(5) 结果都是0
  // 可重现
  val zero = rollDice(SimpleRNG(5))

  def fixRollDice = States.RNG.map(rollDice)(_ + 1)

  def getDices(count: Int) = States.RNG.sequence(List.fill(count)(rollDice))

  // INCREDIBLE!!! 骰子掷10次的结果，传入同一个种子，结果始终一致
  val dices = getDices(10)(SimpleRNG(99))

  //  val ns = States.RNG.Rand[List[Int]] = for{
  //      x <- States.RNG.int  //生成一个整数x
  //      y <- States.RNG.int	//生成另一个整数y
  //      xs <- States.RNG._ints(x) //生成一个列表xs, 长度为x
  //    } yield xs.map(_ % y) //对列表中的每个元素 取余 y


}

object Parallelism {

  object Nonblocking {

    trait Future[+A] {
      private[Parallelism] def apply(k: A => Unit): Unit
    }

    type Par[+A] = ExecutorService => Future[A]

    object Par {

      def run[A](es: ExecutorService)(p: Par[A]): A = {
        val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
        val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
        p(es) { a => ref.set(a); latch.countDown } // Asynchronously set the result, and decrement the latch
        latch.await // Block until the `latch.countDown` is invoked asynchronously
        ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
      }

      def unit[A](a: A): Par[A] =
        es => new Future[A] {
          def apply(cb: A => Unit): Unit =
            cb(a)
        }

      /** A non-strict version of `unit` */
      def delay[A](a: => A): Par[A] =
        es => new Future[A] {
          def apply(cb: A => Unit): Unit =
            cb(a)
        }

      def fork[A](a: => Par[A]): Par[A] =
        es => new Future[A] {
          def apply(cb: A => Unit): Unit =
            eval(es)(a(es)(cb))
        }

      /**
        * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
        * This will come in handy in Chapter 13.
        */
      def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
        def apply(k: A => Unit) = f(k)
      }

      /**
        * Helper function, for evaluating an action
        * asynchronously, using the given `ExecutorService`.
        */
      def eval(es: ExecutorService)(r: => Unit): Unit =
        es.submit(new Callable[Unit] { def call = r })

      def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
        es => new Future[C] {
          def apply(cb: C => Unit): Unit = {
            var ar: Option[A] = None
            var br: Option[B] = None
            // this implementation is a little too liberal in forking of threads -
            // it forks a new logical thread for the actor and for stack-safety,
            // forks evaluation of the callback `cb`
            val combiner = Actor[Either[A,B]](es) {
              //等待另一方完成结果准备
              case Left(a) =>
                if (br.isDefined) eval(es)(cb(f(a,br.get)))
                else ar = Some(a)
              case Right(b) =>
                if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
                else br = Some(b)
            }
            p(es)(a => combiner ! Left(a))
            p2(es)(b => combiner ! Right(b))
          }
        }

      // specialized version of `map`
      def map[A,B](p: Par[A])(f: A => B): Par[B] =
        es => new Future[B] {
          def apply(cb: B => Unit): Unit =
            p(es)(a => eval(es) { cb(f(a)) })
        }

      def lazyUnit[A](a: => A): Par[A] =
        fork(unit(a))

      def asyncF[A,B](f: A => B): A => Par[B] =
        a => lazyUnit(f(a))

      def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
        as match {
          case Nil => unit(Nil)
          case h :: t => map2(h, fork(sequence(t)))(_ :: _)
        }

      def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
        if (as.isEmpty) unit(Vector())
        else if (as.length == 1) map(as.head)(a => Vector(a))
        else {
          val (l,r) = as.splitAt(as.length/2)
          map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
        }
      }

      def sequence[A](as: List[Par[A]]): Par[List[A]] =
        map(sequenceBalanced(as.toIndexedSeq))(_.toList)

      def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] =
        sequence(as.map(asyncF(f)))

      def parMap[A,B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
        sequenceBalanced(as.map(asyncF(f)))

      // exercise answers

      /*
       * We can implement `choice` as a new primitive.
       *
       * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
       * some `Par`, `p`, is the idiom for running `p`, and registering
       * a callback to be invoked when its result is available. The
       * result will be bound to `result` in the function passed to
       * `p(es)`.
       *
       * If you find this code difficult to follow, you may want to
       * write down the type of each subexpression and follow the types
       * through the implementation. What is the type of `p(es)`? What
       * about `t(es)`? What about `t(es)(cb)`?
       */
      def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
        es => new Future[A] {
          def apply(cb: A => Unit): Unit =
            p(es) { b =>
              if (b) eval(es) { t(es)(cb) }
              else eval(es) { f(es)(cb) }
            }
        }

      /* The code here is very similar. */
      def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
        es => new Future[A] {
          def apply(cb: A => Unit): Unit =
            p(es) { ind => eval(es) { ps(ind)(es)(cb) }}
        }

      def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
        choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

      def choiceMap[K,V](p: Par[K])(ps: Map[K,Par[V]]): Par[V] =
        es => new Future[V] {
          def apply(cb: V => Unit): Unit =
            p(es)(k => ps(k)(es)(cb))
        }

      /* `chooser` is usually called `flatMap` or `bind`. */
      def chooser[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
        flatMap(p)(f)

      def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
        es => new Future[B] {
          def apply(cb: B => Unit): Unit =
            p(es)(a => f(a)(es)(cb))
        }

      def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
        flatMap(p)(b => if (b) t else f)

      def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
        flatMap(p)(i => choices(i))

      def join[A](p: Par[Par[A]]): Par[A] =
        es => new Future[A] {
          def apply(cb: A => Unit): Unit =
            p(es)(p2 => eval(es) { p2(es)(cb) })
        }

      def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
        flatMap(a)(x => x)

      def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
        join(map(p)(f))

      /* Gives us infix syntax for `Par`. */
      implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

      // infix versions of `map`, `map2` and `flatMap`
      class ParOps[A](p: Par[A]) {
        def map[B](f: A => B): Par[B] = Par.map(p)(f)
        def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)
        def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
        def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))
      }
    }
  }


  // 实际上， ExecutorService executor = Executors.newCachedThreadPool()
  // executor.submit(callable) => Future ，Future 监控并控制逻辑线程状态
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // `unit` is represented as a function that returns a `UnitFuture`,
  // which is a simple implementation of `Future` that just wraps a constant value.
  // It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled.
  // Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread,
  // in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism.
  // We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    // This implementation of `map2` does _not_ respect timeouts.
    // It simply passes the `ExecutorService` on to both `Par` values,
    // waits for the results of the Futures `af` and `bf`, applies `f` to them,
    // and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation
    // that records the amount of time spent evaluating `af`, then subtracts
    // that time from the available time allocated for evaluating `bf`.
    UnitFuture(f(af.get, bf.get))
  }

  // This is the simplest and most natural implementation of `fork`,
  // but there are some problems with it--for one, the outer `Callable` will block waiting for
  // the "inner" task to complete. Since this blocking occupies a thread in our thread pool,
  // or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism.
  // Essentially, we're using two threads when one should suffice.
  // This is a symptom of a more serious problem with the implementation,
  // and we will discuss this later in the chapter.
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  // A => B 提升到 Par[A] => Par[B]
  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  //组合N个并行计算
  def parMap[A,B](ps: List[A])(f: A => B) = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence_simple(fbs)
  }

//  虽然在实现中调用了fork，
//  由于没有调用run，没有传入ExecutorService，
//  fork无法执行，无论输入的列表有多大，
//  parMap都将立刻返回，当稍后调用到run时，fork将派生出n个并行计算
//  然后等待这些计算结束，结束后收集它们的结果

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map asyncF((a: A) => if (f(a)) List(a) else List()) // A => Par[List[A]]
    // asyncF 意味着每个元素对应一个逻辑线程来判断
    // sequence_simple(pars)  Par[List[[List[A]]]]
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }




}

object Testing{

  type Rand[A] = States.State[RNG, A]

  case class Gen[+A](sample: States.State[RNG,A]) {
    def map[B](f: A => B): Gen[B] =
      Gen(sample.map(f))

    def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
      Gen(sample.map2(g.sample)(f))

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))

    /* A method alias for the function we wrote earlier. */
    def listOfN(size: Int): Gen[List[A]] =
      Gen.listOfN(size, this)
//
//    /* A version of `listOfN` that generates the size to use dynamically. */
    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size flatMap (n => this.listOfN(n))
//
//    def listOf: SGen[List[A]] = Gen.listOf(this)
//    def listOf1: SGen[List[A]] = Gen.listOf1(this)
//
//    def unsized = SGen(_ => this)

    def **[B](g: Gen[B]): Gen[(A,B)] =
      (this map2 g)((_,_))
  }

  object Gen {
    def unit[A](a: => A): Gen[A] =
      Gen(States.State.unit(a))

    val boolean: Gen[Boolean] =
      Gen(State(RNG.boolean))

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

    val uniform: Gen[Double] = Gen(State(RNG.double))

    def choose(i: Double, j: Double): Gen[Double] =
      Gen(State(RNG.double).map(d => i + d*(j-i)))

    /* Basic idea is to add 1 to the result of `choose` if it is of the wrong
     * parity, but we require some special handling to deal with the maximum
     * integer in the range.
     */
    def even(start: Int, stopExclusive: Int): Gen[Int] =
      choose(start, if (stopExclusive%2 == 0) stopExclusive - 1 else stopExclusive).
        map (n => if (n%2 != 0) n+1 else n)

    def odd(start: Int, stopExclusive: Int): Gen[Int] =
      choose(start, if (stopExclusive%2 != 0) stopExclusive - 1 else stopExclusive).
        map (n => if (n%2 == 0) n+1 else n)

    def sameParity(from: Int, to: Int): Gen[(Int,Int)] = for {
      i <- choose(from,to)
      j <- if (i%2 == 0) even(from,to) else odd(from,to)
    } yield (i,j)

    def listOfN_1[A](n: Int, g: Gen[A]): Gen[List[A]] =
      List.fill(n)(g).foldRight(unit(List[A]()))((a,b) => a.map2(b)(_ :: _))

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(b => if (b) g1 else g2)

    def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
      /* The probability we should pull from `g1`. */
      val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

      Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
    }

    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

    /* Not the most efficient implementation, but it's simple.
     * This generates ASCII strings.
     */
    def stringN(n: Int): Gen[String] =
      listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

    val string: SGen[String] = SGen(stringN)

    implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)

    val smallInt = Gen.choose(-10,10)
//    val maxProp = forAll(listOf(smallInt)) { l =>
//      val max = l.max
//      !l.exists(_ > max) // No value greater than `max` should exist in `l`
//    }

    def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))

//    val maxProp1 = forAll(listOf1(smallInt)) { l =>
//      val max = l.max
//      !l.exists(_ > max) // No value greater than `max` should exist in `l`
//    }

    // We specify that every sorted list is either empty, has one element,
    // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
//    val sortedProp = forAll(listOf(smallInt)) { l =>
//      val ls = l.sorted
//      l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a,b) => a > b }
//    }

    object ** {
      def unapply[A,B](p: (A,B)) = Some(p)
    }

    /* A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel
     * computation for each element of the input list summed to produce the final
     * result. This is not the most compelling example, but it provides at least some
     * variation in structure to use for testing.
     *
     * Note that this has to be a `lazy val` because of the way Scala initializes objects.
     * It depends on the `Prop` companion object being created, which references `pint2`.
     */
//    lazy val pint2: Gen[Par[Int]] = choose(-100,100).listOfN(choose(0,20)).map(l =>
//      l.foldLeft(Par.unit(0))((p,i) =>
//        Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))

    def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
      g map (i => (s => i))
  }

  case class SGen[+A](g: Int => Gen[A]) {
    def apply(n: Int): Gen[A] = g(n)

    def map[B](f: A => B): SGen[B] =
      SGen { g(_) map f }

    def flatMap[B](f: A => SGen[B]): SGen[B] = {
      val g2: Int => Gen[B] = n => {
        g(n) flatMap { f(_).g(n) }
      }
      SGen(g2)
    }

    def **[B](s2: SGen[B]): SGen[(A,B)] =
      SGen(n => apply(n) ** s2(n))
  }

  object Prop {
    type SuccessCount = Int
    type TestCases = Int
    type MaxSize = Int
    type FailedCase = String

    sealed trait Result {
      def isFalsified: Boolean
    }
    case object Passed extends Result {
      def isFalsified = false
    }
    case class Falsified(failure: FailedCase,
                         successes: SuccessCount) extends Result {
      def isFalsified = true
    }
    case object Proved extends Result {
      def isFalsified = false
    }


    /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
//    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
//      Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

//    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
//      (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
//        case (a, i) => try {
//          if (f(a)) Passed else Falsified(a.toString, i)
//        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
//      }.find(_.isFalsified).getOrElse(Passed)
//    }


    // String interpolation syntax. A string starting with `s"` can refer to
    // a Scala value `v` as `$v` or `${v}` in the string.
    // This will be expanded to `v.toString` by the Scala compiler.
    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    def apply(f: (TestCases,RNG) => Result): Prop =
      Prop { (_,n,rng) => f(n,rng) }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
      forAll(g(_))(f)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max,n,rng) =>
        val casesPerSize = (n - 1) / max + 1
        val props: Stream[Prop] =
          Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop =
          props.map(p => Prop { (max, n, rng) =>
            p.run(max, casesPerSize, rng)
          }).toList.reduce(_ && _)
        prop.run(max,n,rng)
    }

    def run(p: Prop,
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)): Unit =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
        case Proved =>
          println(s"+ OK, proved property.")
      }

//    val ES: ExecutorService = Executors.newCachedThreadPool
//    val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
//      Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

    def check(p: => Boolean): Prop = Prop { (_, _, _) =>
      if (p) Passed else Falsified("()", 0)
    }

//    val p2 = check {
//      val p = Par.map(Par.unit(1))(_ + 1)
//      val p2 = Par.unit(2)
//      p(ES).get == p2(ES).get
//    }

    def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
      Par.map2(p,p2)(_ == _)

//    val p3 = check {
//      equal (
//        Par.map(Par.unit(1))(_ + 1),
//        Par.unit(2)
//      ) (ES) get
//    }
//
//    val S = weighted(
//      choose(1,4).map(Executors.newFixedThreadPool) -> .75,
//      unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

//    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
//      forAll(S.map2(g)((_,_))) { case (s,a) => f(a)(s).get }
//
//    def checkPar(p: Par[Boolean]): Prop =
//      forAllPar(Gen.unit(()))(_ => p)
//
//    def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
//      forAll(S ** g) { case (s,a) => f(a)(s).get }
//
//    def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
//      forAll(S ** g) { case s ** a => f(a)(s).get }

//    val pint = Gen.choose(0,10) map (Par.unit(_))
//    val p4 =
//      forAllPar(pint)(n => equal(Par.map(n)(y => y), n))
//
//    val forkProp = Prop.forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"
  }

  case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
    def &&(p: Prop) = Prop {
      (max,n,rng) => run(max,n,rng) match {
        case Passed | Proved => p.run(max, n, rng)
        case x => x
      }
    }

    def ||(p: Prop) = Prop {
      (max,n,rng) => run(max,n,rng) match {
        // In case of failure, run the other prop.
        case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
        case x => x
      }
    }

    /* This is rather simplistic - in the event of failure, we simply prepend
     * the given message on a newline in front of the existing message.
     */
    def tag(msg: String) = Prop {
      (max,n,rng) => run(max,n,rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
    }
  }

}

object ScalaThinking {


}
