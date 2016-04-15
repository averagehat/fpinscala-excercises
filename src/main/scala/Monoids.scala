/*
 A monoid is algebra containing:
   1. A type `A`
   2. An operation of type A -> A -> A that is associative.
      op: (A, A) => A
      associative means:
         (x `op` y) `op` z == x `op` (y `op` z)

   3. There is some value `zero` within type A that is the identity for op.
 some value  zero in A where
       (x `op` zero) == x
    an  (zero `op` x) == x

Addition is a monoid.
 Integers form a monoid under addition.

 1. Int
 2. (+) :: A -> A -> A (Int -> Int -> Int) (Int, Int) => Int
   (x + y) + z = x + (y + z)
 3. 0 <- {Int}
   (x + 0) = x  && (0 + x) = x

1. Boolean
2. && (x && y) && z == x && (y && z)
3. True

1. Boolean
2. ||
3. False

 */
object Monoid {

  
 trait Monoid[A] {
   def op(a1: A, a2: A): A
   def zero: A
 }

 val intAddition: Monoid[Int] = new Monoid[Int] {
   def op(x: Int, y: Int): Int = x + y
   val zero = 0
 }

  def listConcatenation[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(xs: List[A], ys: List[A]) = xs ++ ys
    val zero = List() 
  }
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    val zero = None
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)

  def foldRightM[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
  def foldMonoid: Monoid[Either[A,B]] = new Monoid[Either[A,B]] {
    def op(x: Either[A,B], y: Either[A,B]): Either[A,B] = (x, y) match {
      //case (Left(a), Right(b)) => Right(f(a, b))
      case (Right(b), Left(a)) => Right(f(a, b))
    }
    val zero = Right(z)
  }
    foldMap(as, foldMonoid)(x => Left(x)).right.get
  }
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length > 1) {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
    else f(v.head)
  }
  def isOrdered[A <% Ordered[A]](v: IndexedSeq[A]): Boolean = {
    def orderMonoid: Monoid[Option[A]] = new Monoid[Option[A]] {
      def op(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match
        {
          case (Some(x), Some(y)) if (y >= x) => Some(y)
          case _ => None
        }

      val zero = None
    }
    !foldMapV(v, orderMonoid)(Some(_)).isEmpty
  } 
  def main(args: Array[String]): Unit = { 
      println(concatenate(List(List(1, 2), List(3, 4)), listConcatenation[Int]))
      println(concatenate(List(1, 2, 3, 4, 5), intAddition))
      println(concatenate(List(None, Some("foo"), Some("notme")), optionMonoid[String]))
       println(foldRightM(List(1,2,3,4,5), 0)(_ + _))
       println(foldMapV(IndexedSeq(1, 2, 3, 4), optionMonoid[Int])(x => Some(x)))
       println(foldMapV(IndexedSeq(1, 2, 3, 4), listConcatenation[Int])(x => List(x)))
    println(foldMapV(IndexedSeq(1, 2, 3, 4).map(_.toString), intAddition)(_.toInt))
    println(isOrdered(IndexedSeq(1, 2, 3, 4)))
    println(isOrdered(IndexedSeq(1, 2, 3, 1)))
    println(isOrdered(IndexedSeq(3, 2, 1)))

    println(wc("lorem ipsum foo"))
    println(wc(" lorem ipsum foo "))
    
  }

  def wcMonoid = new Monoid[WC] {
    def op(x: WC, y: WC): WC = (x, y) match {
      case  (Stub(a), Stub(b)) => Stub(a + b)
      case  (Part(l, n, r), Stub(b)) => Part(l, n, r + b)
      case  (Stub(b), Part(l, n, r)) => Part(b + l, n, r)
      case  (Part(l1, n1, r1), Part(l2, n2, r2)) => {
        //val count = if (r1.contains(" ") && r2.tail.contains(" ") ||
        //      r1.init.contains(" ") && r2.contains(" ")) (n1 + n2 + 1)
        println(x + ", " + y)
        val count = if (r1.contains(" ") && r2.contains(" ")) (n1 + n2 + 1)
        else (n1 + n2)
        Part(l1, count, r2)
      }
    }
    val zero  = Stub("")
  }
  val minSize = 10
  def wc(s: String): Int = {
    def wordCount(s: String): WC = {
      if (s.length >= minSize) {
        val (l, r) = s.splitAt(s.length / 2)
        wcMonoid.op(wordCount(l), wordCount(r))
      }
      else
        if (!s.contains(" ")) Stub(s)
      else {
        val parts = s.split(" ")
        if (parts.length > 2)
        Part(parts.head + " ", parts.length - 2, " " + parts.last) 
        else Part(parts.head, parts.length - 2, parts.last)
          }
    }
    wordCount(s) match {
    case  Stub(_) => 0
    case  Part(_, n, _) => n
    }
  }
}
sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC
