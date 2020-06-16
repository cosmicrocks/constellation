package org.constellation.playground.schema

import cats.Monoid
import higherkindness.droste.{CVAlgebra, Coalgebra}
import higherkindness.droste.data.:<

abstract class Hom extends Monoid[Hom] {
  val coalgebra: Coalgebra[Option, Fiber]
  val algebra: CVAlgebra[Option, Fiber]
}
//todo at some point we need a parent type bound. We should organize:
// Snapshot <: CheckpointBlock <: Tx <: Hom and Hom can carry the monoids for forming edges. Note that this hierarchy
// is equivalent to:
// converged full state picture <: graph/hypergraph link <: linked list (topological ordering) <: user defined type class

//I'd have users define their typeclasses by extending Fiber, then with ^ hierarchy, should implicitly carry through
case class Fiber() extends Hom {
  val coalgebra: Coalgebra[Option, Fiber] =
    Coalgebra(n => Some(this))

  val algebra: CVAlgebra[Option, Fiber] = CVAlgebra {
    case Some(r1 :< Some(r2 :< _)) => this
    case Some(_ :< None)           => this
    case None                      => this
  }

  override def combine(x: Hom, y: Hom): Hom = x

  override def empty: Hom = this
}

object Fiber {}
