package org.constellation.playground.schema

import cats.Monoid
import higherkindness.droste.{CVAlgebra, Coalgebra}

//todo: We should organize:
// Snapshot <: CheckpointBlock <: Tx <: Hom and Hom can carry the monoids for forming edges. Note that this hierarchy
// is equivalent to:
// converged full state picture <: graph/hypergraph link <: linked list (topological ordering) <: user defined type class
abstract class Hom extends Monoid[Hom] {//needs to be free http://eed3si9n.com/herding-cats/Free-monads.html
  val coalgebra: Coalgebra[Option, Fiber]
  val algebra: CVAlgebra[Option, Fiber]
}


//todo Define Operad like Hom but using MonoidK and for Cell/CoCell.
trait Operad

abstract class Yoneda[F[_], A] { self =>
  def transformation[B](f: A => B): F[B]

  def run: F[A] = transformation(identity)

  def map[B](f: A => B): Yoneda[F, B] = new Yoneda[F, B] {
    def transformation[C](g: (B) => C): F[C] = self.transformation(g compose f)
  }
}