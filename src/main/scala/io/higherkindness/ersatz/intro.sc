import cats._
import cats.implicits._

def foldRight0[E, B](input: List[E])(z: B)(op: (E, B) => B): B = {
  input match {
    case Nil          => z
    case head :: tail => op(head, foldRight0(tail)(z)(op))
  }
}

val prodOp: (Int, Int) => Int = _ * _

foldRight0(1 :: 10 :: 20 :: Nil)(1)(prodOp)

def foldRight1[E, B](z: B)(op: (E, B) => B): List[E] => B = {
  lazy val kernel: List[E] => B = {
    case Nil          => z
    case head :: tail => op(head, kernel(tail))
  }
  kernel
}

foldRight1(1)(prodOp)(1 :: 10 :: 20 :: Nil)

def foldRight2[E, B](z: B)(op: (E, B) => B): List[E] => B = {
  new (List[E] => B) { kernel =>
    def apply(input: List[E]): B = {
      input match {
        case Nil          => z
        case head :: tail => op(head, kernel(tail))
      }
    }
  }
}

foldRight2(1)(prodOp)(1 :: 10 :: 20 :: Nil)

def foldRight3[E, B](f: Option[(E, B)] => B): List[E] => B =
  new (List[E] => B) { kernel =>
    def apply(init: List[E]): B = init match {
      case Nil          => f(None)
      case head :: tail => f(Some((head, kernel(tail))))
    }
  }

val prodF: Option[(Int, Int)] => Int = {
  _ match {
    case None         => 1
    case Some((x, y)) => x * y
  }
}

foldRight3(prodF)(1 :: 10 :: 20 :: Nil)

/*
def foldRight4[E, B](f: Option[(E, B)] => B): List[E] => B = {
  new (List[E] => B) { kernel =>
    def apply(init: List[E]): B = init match {
      case Nil          => f(None)
      case head :: tail => f(Some((head, kernel(tail))))
    }
  }
}

foldRight4(prodF)(1 :: 10 :: 20 :: Nil)
 */

def foldRight4[E, B](f: Option[(E, B)] => B): List[E] => B = {
  new (List[E] => B) { kernel =>
    def step1: List[E] => Option[(E, List[E])] = {
      _ match {
        case Nil          => None
        case head :: tail => Some((head, tail))
      }
    }
    def step2: Option[(E, List[E])] => Option[(E, B)] = {
      _ match {
        case None          => None
        case Some((e, le)) => Some((e, kernel(le)))
      }
    }

    def apply(init: List[E]): B = f(step2(step1(init)))
  }
}

foldRight4(prodF)(1 :: 10 :: 20 :: Nil)


def foldRight5[E, B](f: Option[(E, B)] => B): List[E] => B = {

  type F[P] = Option[(E, P)]
  type S = List[E]

  new (S => B) { kernel =>
    def step1: S => F[S] = {
      _ match {
        case Nil          => None
        case head :: tail => Some((head, tail))
      }
    }
    def step2: F[S] => F[B] = {
      _ match {
        case None          => None
        case Some((e, le)) => Some((e, kernel(le)))
      }
    }

    def apply(init: S): B = f(step2(step1(init)))
  }
}

foldRight5(prodF)(1 :: 10 :: 20 :: Nil)

type ListF[A, B] = Option[(A, B)]

implicit def functor[A]: Functor[ListF[A, ?]] = Functor[Option].compose[(A, ?)]

def projectList[E]: List[E] => ListF[E, List[E]] = {
  _ match {
    case Nil          => None
    case head :: tail => Some((head, tail))
  }
}

def foldRight6[F[_]: Functor, S, B](f: F[B] => B)(project: S => F[S]): S => B = {
  new (S => B) { kernel =>
    def apply(init: S): B = f(project(init).fmap(kernel))
  }
}

val prodFlist: ListF[Int, Int] => Int = {
  _ match {
    case None         => 1
    case Some((x, y)) => x * y
  }
}

foldRight6(prodFlist)(projectList).apply(1 :: 10 :: 20 :: Nil)

def embedList[E]: ListF[E, List[E]] => List[E] = {
  _ match {
    case None => Nil
    case Some((e, le)) => e :: le
  }
}

val range: Int => ListF[Int, Int] =
  v => if (v <= 0) None else Some((v, v - 1))

def unfold[F[_]: Functor, S, A](f: (A) => F[A])(embed: F[S] => S): A => S =
  new (A => S) { kernel =>
    def apply(init: A): S = embed(f(init).fmap(kernel))
  }

unfold(range)(embedList).apply(10)

def cata0[F[_]: Functor, S, B](f: F[B] => B)(project: S => F[S]): S => B =
  new (S => B) { kernel =>
    def apply(init: S): B = f(project(init).fmap(kernel))
  }

def ana0[F[_]: Functor, S, A](f: (A) => F[A])(embed: F[S] => S): A => S =
  new (A => S) { kernel =>
    def apply(init: A): S = embed(f(init).fmap(kernel))
  }



type Algebra[F[_], A] = F[A] => A
type Coalgebra[F[_], A] = A => F[A]

val productOpA: Algebra[ListF[Int, ?], Int] = {
  case None => 1
  case Some((x, y)) => x * y
}
val rangeOpC: Coalgebra[ListF[Int, ?], Int] =
  n => if (n <= 0) None else Some((n, n - 1))


def cata[F[_]: Functor, S, B](algebra: Algebra[F, B])(project: Coalgebra[F, S]): S => B =
  new (S => B) { kernel =>
    def apply(input: S): B =
      algebra(project(input).fmap(kernel))
  }

def ana[F[_]: Functor, S, A](coalgebra: Coalgebra[F, A])(embed: Algebra[F, S]): A => S =
  new (A => S) { kernel =>
    def apply(init: A): S =
      embed(coalgebra(init).fmap(kernel))
  }

def projectListC[A]: Coalgebra[ListF[A, ?], List[A]] = {
  case Nil => None
  case head :: tail => Some((head, tail))
}

def embedListA[A]: Algebra[ListF[A, ?], List[A]] = {
  case None => Nil
  case Some((head, tail)) => head :: tail
}

cata(productOpA)(projectListC).apply(1 :: 10 :: 20 :: Nil)
ana(rangeOpC)(embedListA).apply(10)



{
  val rangeList: Int => List[Int] = ana(rangeOpC)(embedListA)
  rangeList(10)
  val productList: List[Int] => Int = cata(productOpA)(projectListC)
  productList(1 :: 10 :: 20 :: Nil)
}

{
  val rangeList: Int => List[Int] = ana(rangeOpC)(embedListA)
  val productList: List[Int] => Int = cata(productOpA)(projectListC)
  val factorial: Int => Int = productList compose rangeList
  factorial(4)
}

def hylo[F[_]: Functor, A, B](algebra: Algebra[F, B], coalgebra: Coalgebra[F, A]): A => B =
  new (A => B) { kernel =>
    def apply(init: A): B =
      algebra(coalgebra(init).fmap(kernel))
  }

{
  val factorial: Int => Int = hylo(productOpA, rangeOpC)
  factorial(4)
}