package org.constellation.playground.schema

import cats.{Applicative, Eval, Traverse}
import higherkindness.droste.{CVAlgebra, Coalgebra}

//todo put edge/signing stuff here
trait Hom[A] extends Operad {
//  val coalgebra: Coalgebra[Hom, Hom[A]] = ??? //todo unimplement/make interface
//  val algebra: CVAlgebra[Hom, Hom[A]] = ???
}

case class Context(database: String)

//todo think of as Semigroup ADT for joining data
abstract class Fiber[A] extends Hom[A] {
  def unit: Hom[A] //todo for flattening action chains
}

//todo think of as Monoid ADT for Edges to be merged
abstract class Bundle[F](fibers: F) extends Fiber[F]

object Bundle {//use endo to define mixes of parents/children for Edges
  implicit val traverseInstance: Traverse[Bundle] = new Traverse[Bundle]{//todo use enrichment here if needed
    override def traverse[G[_], A, B](fa: Bundle[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Bundle[B]] = ???

    override def foldLeft[A, B](fa: Bundle[A], b: B)(f: (B, A) => B): B = ???

    override def foldRight[A, B](fa: Bundle[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
  }
}

//todo think of as MonoidK ADT representations of an entire state dag converged (cell results)
abstract class Simplex[T](fibers: Seq[Bundle[T]]) extends Bundle[Fiber[T]](fibers.head)//todo fold to get product
