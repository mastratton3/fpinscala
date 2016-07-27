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

case class Prop(run: (TestCases, RNG) => Result)


object Prop {

  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int

  /** Need to go implement more code in laziness */
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
    randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if(f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a,e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String = {
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
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
}

import Gen._

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = 
    Gen(sample.map(a => f(a)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = 
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def listOfN(size: Int): Gen[List[A]] = listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap{
    n =>
    listOfN(n)
  }


}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.nonNegativeInt).map(x => if(x % 2 == 1) true else false))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }  

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap{
    x =>
    if(x) g1
    else g2
  }


}


trait SGen[+A] {

}

