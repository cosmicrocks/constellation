package org.constellation.playground.schema

import cats.{Applicative, Eval, Traverse}
import higherkindness.droste.{CVAlgebra, Coalgebra}

//todo put edge/signing stuff here
trait Hom[A] extends Operad {
  val data: A
  val coalgebra: Coalgebra[Hom, A]
  val algebra: CVAlgebra[Hom, A]
  def product(x: Operad, y: Operad): Operad = product(x, y)
  def tensor(x: Operad, y: Operad): Operad = tensor(x, y)

  def product(x: Hom[_], y: Hom[_]): Hom[_]
  //todo need for group action https://ncatlab.org/nlab/show/action and to show plan
  def tensor(x: Hom[_], y: Hom[_]): Hom[_] // == compose.flatmap(product)

  //todo def endo -> The endomorphism operad composition is obtained by tensoring this last arrow with hom <=> Gets nested Cell Traversal. Flatten via Traverse or enrichment
  // https://ncatlab.org/nlab/show/endomorphism+ring ^, this is a Fixpoint, we can nest/chain these, like hypergraph.
  def endo: Hom[A]

  def unit: Hom[A] //todo for flattening action chains
}

case class Context(database: String)

//todo think of as Semigroup ADT for joining data
abstract class Fiber[A] extends Hom[A]
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
abstract class Simplex[T](fibers: Seq[Bundle[T]]) extends Bundle[T](fibers.head.data)//todo fold/endo to get product

object Simplex {
  //todo methods for creating mixed parent edges across Bundles of converged bundles
}
