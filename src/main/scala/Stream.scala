import scala.collection.immutable.Stream
import scala.{Option => _, Some => _, Either => _, _}
//import scalaz.{\/-, -\/, \/}
trait Stream[+A] {
  import Stream._
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList(): List[A] = this.foldRight(List():List[A])(_ +: _)

  def take(n: Int): Stream[A] = (this, n) match {
    case (_, 0) => Empty
    //Can't use the Cons() constructor directly cuz type stuff.
    case (Cons(h, t), n) => cons(h(), t().take(n - 1))
    case (Empty, _) => throw new RuntimeException("Tried to take too much.")
  }

  def drop(n: Int): Stream[A] = (n, this) match {
    case (0, xs) => xs
    case (n, Cons(h, t)) => t().drop(n-1)
    case (_, Empty) => sys.error("tried to drop too much")
  }

  //  def takeWhile(p: A => Boolean): Stream[A] = this match {
  //    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
  //    case _ => Empty
  //    }

  // this works because foldRight goes backwards,
  // evaluating the end of the list first!
  // and it's lazy because `xs` is only evaluated (and the recursion only continues)
  // if p(x) is true. Once it's false, it stops, evaluating x (or h), and returning empty!
  def takeWhile(p: A => Boolean): Stream[A] =
    this.foldRight(empty:Stream[A])(
      (x, xs) => if (p(x)) cons(x, xs) else empty)
  //  this.foldRight((empty:Stream[A], true))(
  //      { case (x, (xs, b)) => if (p(x) && b) (cons(x, xs), true) else (xs, false)
  //      case _ => sys.error("bad takeWhile!")})._1

  /*
  here is some docs
   */
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
  }

//  def headEither: String \/ A =
//    this.foldRight(-\/("not there"):String \/ A)(
//      (h, e) => if (h == empty) e else \/-(h))

  def headOption: Option[A] =
    this.foldRight(None:Option[A])((h, _) => if (h == Empty) None else Some(h))

  def map[B](f: (A => B)): Stream[B] =
    this.foldRight(empty[B])((x, z) => cons(f(x), z))


  def filter(p: A => Boolean): Stream[A] =
    this.foldRight(empty[A])(
      (x, z) => if (p(x)) cons(x, z) else z)

  //def append[B <: A](ys: Stream[B]): Stream[B]
  def append[B >: A](ys: Stream[B]): Stream[B] =
    ys.foldRight(this:Stream[B])(
      (h, zs) => cons(h, zs))

  def flatMap[B](f: (A => Stream[B])): Stream[B] =
    this.map(f).foldRight(empty[B])( (xs, zs) => zs.append(xs))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
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

  def repeat[A](a: A): Stream[A] = {
    lazy val as:Stream[A] = cons(a, as)
    as
  }
  //def from(n: Int): Stream[Int] = cons(n, from(n+1))

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, f0 + f1))
      go(0, 1)
    }
  }
  //def unfold[A, S](z: S)(f: S => Option[(A, S)]) = {
  //  def go(res: Option[(A, S)]): Stream[A] = res match {
  //    case None => empty
  //    case Some((v, xs)) => cons(v, go(f(xs)))
  //  }
  //  go(f(z))
  //  }

  // take sthe last elem and current elem
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  //def from(n: Int) = unfold(n.)(

}


//fib(5)
//val ones: Stream[Int] = Stream.cons(1, ones)
//ones.exists(1 == _)
//ones.find(1 == _)
//Stream(1, 2, 3, 4).toList()
//Stream(1, 2, 3, 4).takeWhile(_ < 3).toList()
//Stream('a, 'b, 'c, 'd, 'e).take(3).toList()
//Stream('a, 'b, 'c, 'd, 'e).drop(3).toList()
//Stream('a, 'b, 'c, 'd, 'e).forAll(_ == 'a)
//Stream(1). headOption
//Stream(1). headEither
//Stream() .headOption
//Stream() .headEither
//Stream(9,1 ,2, 3). headOption
//val e = Stream(). headEither
//e.validation.toValidationNel
//Stream(1, 2, 3).map(_ + 2).toList()
//Stream(1, 2, 3).filter(_ % 2 == 0).toList()
//Stream(1, 2, 3).append(Stream(4, 5, 6)).toList //(_ % 2 == 0).toList()
//Stream("a", "b","c").flatMap(x => Stream(x, x)).toList
//Stream.repeat(3).take(3).toList() == List(3, 3, 3)
//ones.exists(1 == _)
//ones.find(1 == _)
//Stream(1, 2, 3, 4).toList()
//Stream(1, 2, 3, 4).takeWhile(_ < 3).toList()
//Stream('a, 'b, 'c, 'd, 'e).take(3).toList()
//Stream('a, 'b, 'c, 'd, 'e).drop(3).toList()
//Stream('a, 'b, 'c, 'd, 'e).forAll(_ == 'a)
//Stream(1). headOption
//Stream(1). headEither
//Stream() .headOption
//Stream() .headEither
//Stream(9,1 ,2, 3). headOption
////val e = Stream(). headEither
//e.validation.toValidationNel
//Stream(1, 2, 3).map(_ + 2).toList()
//Stream(1, 2, 3).filter(_ % 2 == 0).toList()
//Stream(1, 2, 3).append(Stream(4, 5, 6)).toList //(_ % 2 == 0).toList()
//Stream("a", "b","c").flatMap(x => Stream(x, x)).toList
//Stream.repeat(3).take(3).toList() == List(3, 3, 3)
//Stream.from(3).take(3).toList //== List(3, 4, 5)
//Stream.fibs(4).toList()
//Stream.from(3).take(3).toList //== List(3, 4, 5)

