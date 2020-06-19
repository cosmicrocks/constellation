package org.constellation.playground.schema

import cats.data.State
import cats.effect.{Concurrent, IO}
import cats.free.Free
import cats.implicits._
import cats.{Applicative, Bimonad, Eval, Functor, Monad, MonoidK, Traverse, ~>}
import org.constellation.playground.schema.Enrichment.{A, M}

import scala.annotation.tailrec
import scala.collection.mutable


case class CellT[F[_] : Concurrent, A](value: A) {}

case class Cell[A](value: A) extends Operad {
  //    def drawPlan: A => String = //todo offline method to draw plan
  def job: State[Cell[_], A] = State.pure(value)
}

object Cell {
  implicit val traverseInstance: Traverse[Cell] = new Traverse[Cell] {
    override def traverse[G[_], A, B](fa: Cell[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Cell[B]] = ???

    override def foldLeft[A, B](fa: Cell[A], b: B)(f: (B, A) => B): B = ???

    override def foldRight[A, B](fa: Cell[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
  }

  implicit val cellMonoid = new MonoidK[Cell] { //todo use MonoidK mixing State[Cell, A] monads across the Enrichment
    override def empty[A]: Cell[A] = ???

    override def combineK[A](x: Cell[A], y: Cell[A]): Cell[A] = ???
  }

  implicit val cellBimonad = new Bimonad[Cell] {
    override def coflatMap[A, B](fa: Cell[A])(f: Cell[A] => B): Cell[B] = Cell(f(fa))

    override def flatMap[A, B](fa: Cell[A])(f: A => Cell[B]): Cell[B] = f.apply(fa.value)

    override def pure[A](x: A): Cell[A] = Cell(x)

    override def extract[A](x: Cell[A]): A = x.value

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Cell[Either[A, B]]): Cell[B] = f(a) match {
      case Cell(either) => either match {
        case Left(a) => tailRecM(a)(f)
        case Right(b) => Cell(b)
      }
    }

    def duplicate[A](fa: Cell[A]): Cell[Cell[A]] = Cell(fa)

    def join[A](ffa: Cell[Cell[A]]): Cell[A] = ffa.value //todo join on monad <=> combine on monoid <=> use monoidK
    //todo Semigroupal https://books.underscore.io/scala-with-cats/scala-with-cats.html#evals-models-of-evaluation
  }
}

/**
  * Morphism object
  *
  * @param a
  * @param f todo make default where f is ghylo
  * @tparam X
  */
case class Morphism[X](a: A, f: A => M[X]) {
  def apply: M[X] = f(a)
}

object Enrichment {
  type A // What we start with
  type B // What we end up with
  type T[_] // Our Traversable collection type
  type M[_] // Our Monad target type
  type FreeMorphism[X] = Free[Morphism, X]

  def lift[X](a: A, func: A => M[X]): FreeMorphism[X] =
    Free.liftF(Morphism(a, func))

  val transformation = new (Morphism ~> M) {
    override def apply[X](o: Morphism[X]): M[X] = o.apply
  }

  def toYoneda[F[_], A](fa: F[A])(implicit F: Functor[F]): Yoneda[F, A] =
    new Yoneda[F, A] {
      override def transformation[B](f: (A) => B): F[B] = F.map(fa)(f)
    }

  def fromYoneda[F[_], A](lf: Yoneda[F, A]): F[A] = lf.run

  implicit class TopEnrichedTraverse[F[_], A](as: F[A]) {

    def topologicalTraverse[B, M[_]](f: A => M[B])(implicit T: Traverse[F], MonadM: Monad[M]): M[F[B]] = {
      case class LazyFunction[X](a: A, f: A => M[X]) {
        def apply: M[X] = f(a)
      }
      type FreeLazyFunction[X] = Free[LazyFunction, X]

      def lift[X](a: A, func: A => M[X]): FreeLazyFunction[X] =
        Free.liftF(LazyFunction(a, func))

      val transformation = new (LazyFunction ~> M) {
        override def apply[X](o: LazyFunction[X]): M[X] = o.apply
      }
      T.traverse(as)(lift(_, f)) //todo organize top within lift
        .foldMap(transformation)
    }
  }

}

object EnrichApp extends App {

  import Cell._
  import Enrichment.TopEnrichedTraverse

  val x = List(5, 4, 3, 2, 1)
  var results = new mutable.MutableList[Int]()

  def g(i: Cell[Int]): IO[Unit] = IO {
    Thread.sleep(i.value * 100)
    results = results :+ i.value
    println(i.value)
  }

  val loadCellMonad = traverseInstance
  //Note: we want topologicalTraverse for Stateful (Ordered) operations. Traverse might be faster for parallel
  //We'll want Arrows when mapping over existing state channels. Need to convert State to Kleisli and vice versa
  //Add convenience methods to Cell to "flatmap" or reduce/fold over Arrows via lift
  val cellTrav = List(Cell[Int](0), Cell[Int](1), Cell[Int](2)).topologicalTraverse(g)
  cellTrav.unsafeRunAsyncAndForget()
}