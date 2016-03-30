package fpinscala.errorhandling

//Verdict: use scalaz either within for comprehensions, where computations depend on eachother;
// then convert /\ to Validation when you want to accumulate errors using |@|.
// see http://stackoverflow.com/questions/21351391/how-to-accumulate-errors-in-either-in-scala
// and https://groups.google.com/forum/#!topic/scalaz/Wnkdyhebo2w
import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter
sealed trait Either[+E,+A] {
  //`lift` converts a function into a map
  // lift f = _.map f
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case Left(y) => Left(y)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(b) => f(b)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this flatMap(x => b)

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap(x => b.map(f(x, _)))

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Right(List()):Either[E, List[B]])((o, l) => f(o).map2(l)(_ +: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
