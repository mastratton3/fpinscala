package fpinscala.state


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

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextNum, nextRNG) = rng.nextInt
    if(nextNum < 0) (-(nextNum + 1), nextRNG) else (nextNum, nextRNG)
  }

  /* Old Impl
  def double(rng: RNG): (Double, RNG) = {
    val (nextNum, nextRNG) = nonNegativeInt(rng)
    (nextNum / (Int.MaxValue.toDouble + 1), nextRNG)
  }
  */

  def double(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(x => (x / (Int.MaxValue.toDouble + 1)))(rng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i,d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = nonNegativeInt(r1)
    ((d,i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count <= 0) (List(), rng)
    else {
      val (i, r) = nonNegativeInt(rng)
      val (xs, r2) = ints(count - 1)(r)
      (i :: xs, r2)
    }
  }
    
  /** Whew fixed bug that was causing issue with sequence */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
    val (l, r1) = ra(rng)
    val (r, r2) = rb(r1)
    (f(l,r), r2)
  }

  /** Do fold left impl later
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???
    */
    /*Replace later w/ fold left 
     * Numbers come out the same, doesn't pass RNG to the next, is this normal?
     * Something seems wrong with this, but it matches what the answer returns
     */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def go(rem: List[Rand[A]], acc: Rand[List[A]]): Rand[List[A]] = rem match {
      case Nil => acc 
      case h :: t => go(t, map2(h, acc){ (x, xs) => x :: xs})
    }
    go(fs, unit(List(): List[A]))
  }
  
  
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      val (v, r) = f(rng)
      g(v)(r)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = 
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
