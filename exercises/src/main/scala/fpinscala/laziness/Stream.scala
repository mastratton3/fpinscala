package fpinscala.laziness

import Stream._
trait Stream[+A] {

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
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1 => t().drop(n-1)
    case Cons(_, t) if n == 1 => t()
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) => t().forAll(p)
    case Cons(h, _) if !p(h()) => false
    case empty => true
  }

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case empty => None
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  /* Reimplement later using foldRight and then again with unfold */
  def map[B](f: A => B): Stream[B] = this match {
      case Cons(h, t) => cons(f(h()), t().map(f))
      case _ => empty
  }
  
  /* Later write zipWith and implement using that */
  def zip[B](s2: Stream[B]): Stream[(A,B)] = (this, s2) match {
      case (Cons(h, t), Cons(h2, t2)) => cons((h(), h2()), t().zip(t2()))
      case _ => empty
  }

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

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a,s)) => cons(a, unfold(s)(f))
      case _ => empty
    }
  }  
  
}