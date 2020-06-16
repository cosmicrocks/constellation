package org.constellation.playground.schema

import cats.{FlatMap, Functor, Monad}
import cats.data.Kleisli
import cats.effect.{IO, Sync}
import cats.implicits._
import higherkindness.droste.{Algebra, AlgebraM, CVAlgebra, Coalgebra, Gather, Scatter, scheme}

sealed trait Cell[T] {}
  //todo when ppl make their own cell, they need covariance with other types they wanna mix
  //todo we need to form "cell bialgebra" out of A above. We should be able to read it
  //  val algebra: Algebra[Cell, Kleisli[IO, Context, String]]
  //  val coAlgebra: Coalgebra[Cell, Fiber]

case class Result[F <: Fiber](value: F) extends Cell[F] {
  val op: Fiber => Fiber = scheme.ghylo(//note: we should be able to lift here <=> flat/map
    value.algebra.gather(Gather.histo),
    value.coalgebra.scatter(Scatter.ana))
}
case class Lookup[F <: Fiber](value: F) extends Cell[F]{
  val op: Fiber => Fiber = scheme.ghylo(//note: we should be able to lift here <=> flat/map
    value.algebra.gather(Gather.histo),
    value.coalgebra.scatter(Scatter.ana))
}

case class Upsert[F <: Fiber](value: F) extends Cell[F]{
  val op: Fiber => Fiber = scheme.ghylo(//note: we should be able to lift here <=> flat/map
    value.algebra.gather(Gather.histo),
    value.coalgebra.scatter(Scatter.ana))
}

case class Compute[F <: Fiber](value: F) extends Cell[F]{
  val op: Fiber => Fiber = scheme.ghylo(//note: we should be able to lift here <=> flat/map
    value.algebra.gather(Gather.histo),
    value.coalgebra.scatter(Scatter.ana))
}

abstract class Yoneda[F[_], A] { self =>
  def transformation[B](f: A => B): F[B]

  def run: F[A] = transformation(identity)

  def map[B](f: A => B): Yoneda[F, B] = new Yoneda[F, B] {
    def transformation[C](g: (B) => C): F[C] = self.transformation(g compose f)
  }
}

object Cell {
  type K[F[_], A] = Kleisli[F, Cell[A], Fiber]

  case class Context(database: String)

  implicit val cellFunctorImpl: Functor[Cell] = new Functor[Cell] {
    override def map[A, B](fa: Cell[A])(f: A => B): Cell[B] = fa map f
  }


  def toYoneda[F[_], A](fa: F[A])(implicit F: Functor[F]): Yoneda[F, A] =
    new Yoneda[F, A] {
      override def transformation[B](f: (A) => B): F[B] = F.map(fa)(f)
    }

  def fromYoneda[F[_], A](lf: Yoneda[F, A]): F[A] = lf.run
  val lazyCell: Yoneda[Cell, Fiber] = toYoneda(Compute(Fiber()))

  implicit val cellFlatMapImpl: FlatMap[Cell] = new FlatMap[Cell] {
    override def flatMap[A, B](fa: Cell[A])(f: A => Cell[B]): Cell[B] = fa match {
      case Result(a)  => f(a)
      case Lookup(a)  => f(a)
      case Upsert(a)  => f(a)
      case Compute(a) => f(a)
    }

    override def tailRecM[A, B](a: A)(f: A => Cell[Either[A, B]]): Cell[B] = ???

    override def map[A, B](fa: Cell[A])(f: A => B): Cell[B] = cellFunctorImpl.map(fa)(f)
  }


  def lookup[F[_]](s: String)(implicit F: Sync[F]): Kleisli[F, Context, String] = Kleisli.apply { ctx =>
    F.pure(s"{ s: ${s} | lookup for db: ${ctx.database} }")
  }

  def upsert[F[_]]: Kleisli[F, Context, Fiber] = ???

  def compute[F[_]]: Kleisli[F, Context, Fiber] = ???

}
