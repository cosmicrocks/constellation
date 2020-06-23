package org.constellation.playground.schema

import cats.{Applicative, Eval, Monoid, MonoidK, Traverse}
import higherkindness.droste.{CVAlgebra, Coalgebra}

//todo put edge/signing stuff here
trait Hom[A] extends Operad { self =>
  val data: A = self.unit.data
  val coalgebra: Coalgebra[Hom, A] = Coalgebra(_ => unit)
  val algebra: CVAlgebra[Hom, A] = CVAlgebra {
    case o: Operad => unit.data
  }
  override def tensor(x: Operad, y: Operad): Operad = tensor(x, y)

  def tensor(x: Hom[_], y: Hom[_] = this): Hom[A] = unit

  override def unit: Hom[A] = this
}

object Hom {
  implicit val traverseInstance: Traverse[Hom] = new Traverse[Hom]{
    override def traverse[G[_], A, B](fa: Hom[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Hom[B]] = ???

    override def foldLeft[A, B](fa: Hom[A], b: B)(f: (B, A) => B): B = ???

    override def foldRight[A, B](fa: Hom[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
  }

  implicit val fiberMonoid: MonoidK[Hom] = new MonoidK[Hom]{
    override def empty[A]: Hom[A] = new Hom[A] {}

    override def combineK[A](x: Hom[A], y: Hom[A]): Hom[A] = new Hom[A] {}.tensor(x, y)
  }
}

case class Context(database: String)

//todo think of as Semigroup ADT for joining data
abstract class Fiber[A] extends Hom[A]

//todo think of as Monoid ADT for Edges to be merged
abstract class Bundle[F](fibers: F) extends Fiber[F]

//todo think of as MonoidK ADT representations of an entire state dag converged (cell results)
abstract class Simplex[T](fibers: Seq[Bundle[T]]) extends Bundle[T](fibers.head.data)//todo fold/endo to get product
