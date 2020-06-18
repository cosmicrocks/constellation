package org.constellation.playground.schema

import cats.data.Kleisli
import cats.effect.IO
import cats.{Monad, Monoid, Traverse, ~>}
import cats.free.Free
import higherkindness.droste.{CVAlgebra, Coalgebra}
import higherkindness.droste.data.:<

abstract class CoCell{}

object CoCell {}

case class Fiber() extends CoCell {
  val coalgebra: Coalgebra[Option, Fiber] =
    Coalgebra(n => Some(this))

  val algebra: CVAlgebra[Option, Fiber] = CVAlgebra {
    case Some(r1 :< Some(r2 :< _)) => this
    case Some(_ :< None)           => this
    case None                      => this
  }
}

object Fiber {}
