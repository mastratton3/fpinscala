package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  def intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    def zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 compose a2
    def zero = (x: A) => x
  }

  // We can get the dual of any monoid just by flipping the `op`.
  // Copied from answers.. nothing is ever mentioned of this in the book
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  import fpinscala.testing._
  import Prop._

  /** Originally wrote this a bit differently using a much less elegant solution... ended up just adapting the answer */
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    forAll {
      for {
        i <- gen
        j <- gen
        k <- gen} yield (i, j, k)
    } (p => m.op(m.op(p._1, p._2), p._3) == m.op(p._1, m.op(p._2, p._3))) &&
    forAll(gen) {
      a: A =>
      m.op(m.zero, a) == a && m.op(a, m.zero) == a
    }

  }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero){ case (acc, x) => m.op(acc, f(x)) }

  /** Skipping for now */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.length == 0) m.zero
    else if (as.length == 1) f(as(0))
    else {
      val (l, r) = as.splitAt(as.length/2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  /** Will return to this */
  def ordered(ints: IndexedSeq[Int]): Boolean =
    sys.error("todo")

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC


  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]) = a1.map2(a2)(m.op)
    def zero = Par.unit(m.zero)
  }

  /** Will implement when I return to implement the non-blocking parallelism */
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    sys.error("todo") 

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def zero = Stub("")

    /* Odd, not sure why my implementation isn't compiling
    def op(w1: WC, w2: WC) = (w1, w2) match {
      case (Stub(x), Stub(y)) => Stub(x + y)
      case (Stub(x), Part(l, n r) ) => Part(x + l, n, r)
      case (Part(l, n, r), Stub(x)) => Part(l, n, r + x)
      case (Part(l1, n1, r1), Part(l2, n2, r2)) => 
        Part(l1, n1 + (if ((r1 + l2).isEmpty) 0 else 1) + n2, r2)
    } */
    def op(a: WC, b: WC) = (a, b) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }

  }

  /** Want to come back */
  def count(s: String): Int = sys.error("todo")

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def zero = (A.zero, B.zero)
    def op(l: (A, B), r: (A,B)) = (A.op(l._1, r._1), B.op(l._2, r._2))
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def zero = (a: A) => B.zero
    def op(x: A => B, y: A => B) = (a: A) => B.op( x(a), y(a) )
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K,V]()
    def op(a: Map[K,V], b: Map[K,V]) = (a.keySet ++ b.keySet).foldLeft(zero) {
      (acc, k) => 
      acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
    }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    IndexedSeqFoldable.foldMap(as)((a: A) => Map(a -> 1))(mapMergeMonoid[A, Int](intAddition) )
}

trait Foldable[F[_]] {
  import Monoid._

  /** Don't know if I fully understand this implementation */
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  /** Come back to review, totally need to understand the syntax better here */
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a,b) => mb.op(b, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldLeft(as)(List[A]()){ case (acc, x) => x :: acc}
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b,a) => mb.op(b,f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b,a) => mb.op(b,f(a)))
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(x) => f(x)
    case Branch(l,r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(x) => f(z, x)
    case Branch(l, r) => foldLeft(l)(foldLeft(r)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Some(a) => f(a)
    case None => mb.zero
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z 
    case Some(a) => f(z, a)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(a) => f(a, z)
  }
}

