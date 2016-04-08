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

println(concatenate(List(List(1, 2), List(3, 4)), listConcatenation[Int]))
println(concatenate(List(1, 2, 3, 4, 5), intAddition))
println(concatenate(List(None, Some("foo"), Some("notme")), optionMonoid[String]))
}
