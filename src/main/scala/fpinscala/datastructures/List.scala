package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  // def sum(ints: List[Int]): Int = ints match {
  //   case Nil => 0
  //   case Cons(x, xs) => x + sum(xs)
  // }

  // def product(ds: List[Double]): Double = ds match {
  //   case Nil => 1.0
  //   case Cons(x, xs) => x * product(xs)
  // }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => f( h, List.foldRight(t, z)(f) )
    } 
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft( t, f(z,h) )(f)
    }
  }

  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, (b: B) => B)((a, g) => b => g(f(b, a)))(z)
  }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(List.reverse(as), z)((b, a) => f(a, b))
  }

  // product implemented using foldRight. Can it immediately
  // halt the recursion and return 0.0 if it encounters a 0.0?

  // def product(ds: List[Double]): Double = {
  //   foldRight(ds, 1.0)(_ * _)
  // }

  def sum(as: List[Int]): Int = foldLeft(as, 0)( (acc, x) => acc + x )
  def product(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)
  def length(as: List[Int]): Int = foldLeft(as, 0)((_, acc) => acc + 1)
  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())( (x, y) => Cons(y, x) )
  // because Nil can be any type we should strictly identify the type of the empty list!

  // no because foldRight needs to traverse through all the members
  // of the list before it can begin collapsing it. foldRight takes
  // linear-time because each call represents one level of recursion
  // if we can foldRight with a large list we could trigger stack overflow. 
  // To facilitate early termination we need non-strict evaluation 
  // (i.e. we need not evaluate the arguments before calling a function) 

  // what happens when we pass Nil and Cons to foldRight? 
  // It effectively builds the original List using naive recursion. 
  // However this mustn't be effecient for large Lists :thinking_face:

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // The option we would match against is the third. We could also match against
  // the 4th & 5th options, but as Scala evaluates on the first match they are not
  // included. 

  def tail[A](l: List[A]): List[A] = l match {
    // case Nil => Nil
    case Nil => sys.error("Cannot get the tail of an empty List.")
    case Cons(x, xs) => xs
  } 

  def setHead[A](h: A, l: List[A]): List[A] = Cons(h, List.tail(l))
  // probably not good practice to have a deeper error bubble up
  // but i'm lazy!

  def drop[A](l: List[A], n: Int): List[A] =  {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => List.drop(t, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => List.dropWhile(t, f)
    case _ => l
  }

  // Found drop & dropWhile tricky. Should revise that one 

  def append[A](prefix: List[A], suffix: List[A]): List[A] = {
    prefix match {
      case Nil => suffix
      case Cons(h, t) => Cons(h, append(t, suffix))
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of an empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    //  we could also reverse over the contents of a list twice,
    // applying the transformation in the intemediary data structure
    // look up ListBuffers
  }

  def append[A](as: List[A], bs: List[A]): List[A] = {
    foldRight((as), bs)(Cons(_, _))
  }

  def concat[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil: List[A])(append)
  }

  def addOne(as: List[Int]): List[Int] = {
    foldRight(as, Nil: List[Int])((h, t) => Cons(h+1, t))
  }

  def doubleToString(ad: List[Double]): List[String] = {
    foldRight(as, Nil: List[String])((h, t) => Cons(h.toString, t))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))
  }

   def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b )
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }


}

