package io.higherkindness.ersatz

object tutorial {

  import cats._
  import cats.implicits._

  // start with a concrete computation

  // no recursion, use a loop
  def productList0(input: List[Int]): Int = {
    var res = 1
    input.foreach { res *= _ }
    res
  }

  productList0(1 :: 5 :: 10 :: Nil)

  // with recursion
  def productList1(input: List[Int]): Int = input match {
    case Nil => 1
    case head :: tail => head * productList1(tail)
  }

  productList1(1 :: 5 :: 10 :: Nil)

  //
  // we're going to factor out the operation while leaving our code
  // specitic to List
  //
  // this is basically foldRight (sans tail recursion)
  //

  def foldRight0[X, Y](input: List[X])(initial: Y)(op: (X, Y) => Y): Y = input match {
    case Nil => initial
    case head :: tail => op(head, foldRight0(tail)(initial)(op))
  }

  val v0: Int = 1
  val op: (Int, Int) => Int = _ * _

  foldRight0(1 :: 5 :: 10 :: Nil)(v0)(op)

  1 :: 5 :: 10 :: Nil

  ::(1,
    ::(5,
      ::(10,
        Nil)))

  op(1,
    op(5,
      op(10,
        v0)))

  op(1,
    op(5,
      10))

  op(1,
    50)

  50

  def foldRight1[X, Y](initial: Y)(op: (X, Y) => Y): List[X] => Y = {
    lazy val kernel: List[X] => Y = _ match {
      case Nil => initial
      case head :: tail => op(head, kernel(tail))
    }
    kernel
  }

  def foldRight2[X, Y](initial: Y)(op: (X, Y) => Y): List[X] => Y =
    new (List[X] => Y) { kernel =>
      def apply(input: List[X]): Y = input match {
        case Nil => initial
        case head :: tail => op(head, kernel(tail))
      }
    }

  // what if we abstract over our data structure, too?
  // how do we even do this?

  // what parts of the code are specific to List?

  // 1. we're pattern matching on our list to break it apart
  // 2. we dive deeper if we're at a recursive point in the
  //    data structure
  // 3. we do a computation-- either initial or op-- depending
  //    on where we are in the structure

  // steps 2 and 3 depend on our data structure


  // 1. List[X]           --------------------> 1 + (X × List[X])
  // 2. 1 + (X × List[X]) ---[.... kernel]----> 1 + (X × Y)
  // 3. 1 + (X × Y)       -- [initial, op] ---> Y

  // type F[Z] = Either[Unit, (X, Z)]
  // or...     = Option[(X, Z)]
  //

    def foldRight3[X, Y](initial: Y)(op: (X, Y) => Y): List[X] => Y =
      new (List[X] => Y) { kernel =>

        type F[Z] = Option[(X, Z)]

        def step1(input: List[X]): F[List[X]] = ???
        def step2(ctx: F[List[X]]): F[Y] = ???
        def step3(ctx: F[Y]): Y = ???

        def apply(input: List[X]): Y =
          step3(step2(step1(input)))
      }

  def foldRight4[X, Y](initial: Y)(op: (X, Y) => Y): List[X] => Y =
      new (List[X] => Y) { kernel =>

        type F[Z] = Option[(X, Z)]

        def step1(input: List[X]): F[List[X]] = input match {
          case Nil => None
          case head :: tail => Some((head, tail))
        }

        def step2(ctx: F[List[X]]): F[Y] = ctx match {
          case None => None
          case Some((head, tail)) => Some((head, kernel(tail)))
        }

        def step3(ctx: F[Y]): Y = ctx match {
          case None => initial
          case Some((x, y)) => op(x, y)
        }

        def apply(input: List[X]): Y =
          step3(step2(step1(input)))
      }

    def foldRight5[X, Y](initial: Y)(op: (X, Y) => Y): List[X] => Y =
      new (List[X] => Y) { kernel =>

        type F[Z] = Option[(X, Z)]
        implicit val functorF: Functor[F] =
          Functor[Option].compose[(X, ?)]

        def step1(input: List[X]): F[List[X]] = input match {
          case Nil => None
          case head :: tail => Some((head, tail))
        }

        def step2(ctx: F[List[X]]): F[Y] =
          ctx.fmap(kernel)

        def step3(ctx: F[Y]): Y = ctx match {
          case None => initial
          case Some((x, y)) => op(x, y)
        }

        def apply(input: List[X]): Y =
          step3(step2(step1(input)))
      }


    def foldRightAlmostCata0[X, Y](initial: Y)(op: (X, Y) => Y): List[X] => Y =
      new (List[X] => Y) { kernel =>

        type F[Z] = Option[(X, Z)]
        implicit val functorF: Functor[F] =
          Functor[Option].compose[(X, ?)]

        val project: List[X] => F[List[X]] = _ match {
          case Nil => None
          case head :: tail => Some((head, tail))
        }

        val algebra: F[Y] => Y = _ match {
          case None => initial
          case Some((x, y)) => op(x, y)
        }

        def apply(input: List[X]): Y =
          algebra(project(input).fmap(kernel))
      }

  def cata[F[_]: Functor, A, Y](algebra: F[Y] => Y)(project: A => F[A]): A => Y =
    new (A => Y) { kernel =>
      def apply(input: A): Y =
        algebra(project(input).fmap(kernel))
    }

  //


  def rangeList0(a: Int): List[Int] = a match {
    case 0 => Nil
    case n => n :: rangeList0(n - 1)
  }

  rangeList0(10)

  val rangeOp: Int => Option[(Int, Int)] =
    v => {
      if (v > 0) Some((v - 1, v))
      else None
    }

  def unfold0[A, B](input: A)(op: A => Option[(A, B)]): List[B] = {
    op(input) match {
      case None => Nil
      case Some((a, b)) => b :: unfold0(a)(op)
    }
  }

  unfold0(10)(rangeOp)

  def unfold1[A, B](op: A => Option[(A, B)]): A => List[B] = {
    lazy val kernel: A => List[B] = op(_) match {
      case None => Nil
      case Some((a, b)) => b :: kernel(a)
    }
    kernel
  }

  def unfold2[A, B](op: A => Option[(A, B)]): A => List[B] = {
    new (A => List[B]) { kernel =>
      def apply(a: A): List[B] = op(a) match {
        case None => Nil
        case Some((a, b)) => b :: kernel(a)
      }
    }
  }

}
