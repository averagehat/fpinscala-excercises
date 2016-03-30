package fpinscala.state
import scalaz._

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)
//  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = // no, this returns a function map(a)(a => map(b)(f(a,_)))
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  //def nonNegativeInt(rng: RNG): (Int, RNG) = map(rng.nextInt)(x => if (x >= 0) then x else)
  // nonNegativeInt cannot be done using map because it requires a recursive call if it it's negative, and map doesn't
  // provide access to the next state! (i.e. the returned RNG. map only allows f to access the produced A.)

  def double(rng: RNG): (Double, RNG) = map(_.nextInt)(x => x.toDouble / Double.MaxValue)(rng)

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra,rb)((_,_))

  def intDouble(rng: RNG): ((Int,Double), RNG) = both(int, double)(rng)

  def doubleInt(rng: RNG): ((Double,Int), RNG) = both(double, int)(rng)

  def doubleInt: Rand[(Double, Int)] = both(double, int)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = sys.error("todo")

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  //def sequence[A](fs: IList[Rand[A]]): Rand[IList[A]] = fs.foldRight(unit(IList[A]()))(map2(_, _)(_ :: _))
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))(map2(_, _)(_ :: _))

  //assert(sequence(IList(unit(1), unit(2))) == unit(List(1, 2)))
  // fit the words to the types...

  //def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  def flatMap[A,B](ra: Rand[A])(g: A => Rand[B]): Rand[B] = rng => { 
    val (a, rng1) = ra(rng)
    g(a)(rng1)
  }

  //def nonNegativeInt(rng: RNG): (Int, RNG) = flatMap(int)({ case n => if (n >= 0) unit(n) else nonNegativeInt(n)
  def nonNegativeInt(rng: RNG): (Int, RNG) = int(rng) match { case r@(n, rng2) => if (n >= 0) r else nonNegativeInt(rng2) }

  def _map[A,B](r: Rand[A])(f: A => B): Rand[B] = flatMap(r)(f andThen unit) // note this is the only possible implementation because unit is the only way to lift a value into Rand

  //def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = // no, this returns a function map(a)(a => map(b)(f(a,_)))
  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
                                                          val mod = i % n
                                                          if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
                                                        })
  def between(start: Int, stopExclusive: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
              val mod = i % stopExclusive
              if (i + (stopExclusive - 1) - mod >= start) unit(mod) else between(start, stopExclusive) })
}

case class State[S,+A](run: S => (A, S)) {
  import State.unit
  def map[B](f: A => B): State[S, B] = this.flatMap(f andThen unit)
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = this.flatMap(a => sb.map(f(a, _)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, state2) = this.run(s)
      f(a).run(state2)
})
}
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  //Should this return a tuple/State like simulateMachine?
  def nextState(input: Input): Machine = 
    if (coins == 0) this else input match {
      case Coin if (candies > 0) => Machine(false, candies, coins + 1)
      case Turn if !locked => Machine(true, candies - 1, coins)
      case _ => this
    }


}

object State {
  def unit[S,A](a: A): State[S, A] =  State(s => (a, s))
  type Rand[A] = State[RNG, A]
  def map2[S,A,B,C](s1: State[S, A], s2: State[S, B])(f: (A,B) => C): State[S, C] = throw new Error()
  def flatMap[S,A,B](s1: State[S, A])(f: (A => State[S, B])): State[S, B] =
    State( start =>  {
       val (a, next) = s1.run(start)
            f(a).run(next) })

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))(map2(_, _)(_ :: _))
  def simulateMachine(inputs: IList[Input]): State[Machine, (Int, Int)] = State(m => {
                                                                                  val machine = inputs.foldLeft(m)(_ nextState _)
                                                                                  machine match {case Machine(_, x, y) => ((x, y), machine)}
                                                                                })
  }

object Test {
 val m = Machine(true, 5, 10)
 val inputs = IList[Input](Coin, Turn, Coin, Turn, Coin, Turn, Turn, Turn, Coin, Turn)
  val resultState = State.simulateMachine(inputs).run(m)
  val expected = ((1,14), Machine(true,1,14)) 
  assert(resultState == expected)

}
