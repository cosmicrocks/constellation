package org.constellation.playground.schema

import cats.{Applicative, Eval, FlatMap, Foldable, Functor, Monad, Monoid, Traverse, ~>}
import cats.data.Kleisli
import cats.effect.{Concurrent, IO, Sync}
import cats.free.Free
import cats.implicits._
import higherkindness.droste.{Algebra, AlgebraM, CVAlgebra, Coalgebra, Gather, Scatter, scheme}
import org.constellation.playground.schema.Enrichment.{A, M}

import scala.annotation.tailrec
import scala.collection.mutable

case class CellT[F[_]: Concurrent, A](value: A) {
  //todo get def op: A => Fiber from A
  def job: Kleisli[F, A, String] = Kleisli.pure("result")//fold over op, nested ghylo executed within A.op
}

case class Cell[A](value: A){// extends Operad
//    def drawPlan: A => String = //todo offline method to draw plan

  def lookup[F[_]](s: String)(implicit F: Sync[F]): Kleisli[F, Context, String] = Kleisli.apply { ctx =>
    F.pure(s"{ s: ${s} | lookup for db: ${ctx.database} }")
  }

  def upsert[F[_]]: Kleisli[F, Context, Fiber] = ???

  def compute[F[_]]: Kleisli[F, Context, Fiber] = ???

  def stream[F[_]]: Kleisli[F, Context, Fiber] = ???
}

object Cell {
  implicit val traverseInstance: Traverse[Cell] = new Traverse[Cell] {
    override def traverse[G[_], A, B](fa: Cell[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Cell[B]] = ???

    override def foldLeft[A, B](fa: Cell[A], b: B)(f: (B, A) => B): B = ???

    override def foldRight[A, B](fa: Cell[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
  }

  implicit val cellMonadInstance: Monad[Cell] = new Monad[Cell] {//todo introduce CoCell here
    override def pure[A](x: A): Cell[A] = Cell(x)
    override def flatMap[A, B](fa: Cell[A])(f: A => Cell[B]): Cell[B] = f.apply(fa.value)
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Cell[Either[A, B]]): Cell[B] = f(a) match {
      case Cell(either) => either match {
        case Left(a) => tailRecM(a)(f)
        case Right(b) => Cell(b)
      }
    }
  }
}

case class Context(database: String)

//case class Result[F <: Fiber](value: F) extends Cell[F]
//case class Lookup[F <: Fiber](value: F) extends Cell[F]{
//  val op: Fiber => Fiber = scheme.ghylo(//note: we should be able to lift here <=> flat/map
//    value.algebra.gather(Gather.histo),
//    value.coalgebra.scatter(Scatter.ana))
//}
//case class Upsert[F <: Fiber](value: F) extends Cell[F]{
//  val op: Fiber => Fiber = scheme.ghylo(//note: we should be able to lift here <=> flat/map
//    value.algebra.gather(Gather.histo),
//    value.coalgebra.scatter(Scatter.ana))
//}
//case class Compute[F <: Fiber](value: F) extends Cell[F]{
//  val op: Fiber => Fiber = scheme.ghylo(//note: we should be able to lift here <=> flat/map
//    value.algebra.gather(Gather.histo),
//    value.coalgebra.scatter(Scatter.ana))
//}

/**
  * Morphism object
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
        T.traverse(as)(lift(_, f))//todo organize top within lift
          .foldMap(transformation)
      }
    }
}

object EnrichApp extends App {
  import Enrichment.TopEnrichedTraverse
  import Cell._
  val x = List(5, 4, 3, 2, 1)
  var results = new mutable.MutableList[Int]()
  def g(i: Cell[Int]): IO[Unit] = IO{
    Thread.sleep(i.value * 100)
    results = results :+ i.value
    println(i.value)
  }
  val loadCellMonad = traverseInstance
  //Note: we want topologicalTraverse when order matters. Traverse might be faster for parallel
  val cellTrav = List(Cell[Int](0), Cell[Int](1), Cell[Int](2)).topologicalTraverse(g)
  cellTrav.unsafeRunAsyncAndForget()
}