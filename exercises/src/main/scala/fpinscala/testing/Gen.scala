package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

/*
trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  /* Old Code
  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
  */

}
*/


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(_,_) => p.run(max, n, rng)
      case x => x
    }
  }

}


object Prop {

  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  /** Need to go implement more code in laziness */
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
    val res = randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if(f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a,e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
    res
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String = {
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  }
  
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

    /*
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => 

      val casesPerSize = (n + (max - 1)) / max 
      val props: Stream[Prop] = 
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = 
        props.map(p => Prop { (max, _, rng) => 
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
    props.run(max, n, rng)
  }
  */

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
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
  }

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false 
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }


}

import Gen._

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = 
    Gen(sample.map(a => f(a)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = 
    Gen(sample.flatMap(a => f(a).sample))


  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap (n => this.listOfN(n))

  def listOf: SGen[List[A]] = Gen.listOf(this)

  def unsized: SGen[A] = SGen(_ => this)


}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.nonNegativeInt).map(x => if(x % 2 == 1) true else false))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }  

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))


  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => g.listOfN(n))
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => g.listOf(n max 1))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap{
    x =>
    if(x) g1
    else g2
  }

  val smallInt = choose(-10,10)
  val maxProp = forAll(listOf1(smallInt)) { ns => 
    val max = ns.max
    !ns.exists(_ > max)
  }

  val sortedProp = forAll(listOf1(smallInt)) { ns =>

    val max = ns.max
    val min = ns.min
    val sortedList = ns.sorted 

    sortedList.head == min && sortedList.last == max
  }


}


case class SGen[+A](g: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = {
    SGen{ g(_) map f }
  }

  /*
  def flatMap[B](f: A => SGen[B]): SGen[B] = {

  }
  */

}
