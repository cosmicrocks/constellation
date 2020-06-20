package org.constellation.playground.schema

import cats.free.Cofree
import cats.{Eval, MonoidK}
import higherkindness.droste.{CVAlgebra, Coalgebra}
import shapeless.{:+:, CNil, Coproduct, HList, HNil, LabelledGeneric, Lazy, TypeClass}
import shapeless._

case class Cocell[A](value: A, stateTransitionEval: Eval[Cell[Cofree[Cell, A]]])  extends FreeOperad[A] {
  val plan: Cofree[Cell, A] = Cofree[Cell, A](value, stateTransitionEval)
}

object Cocell {
  implicit val cellMonoid = new MonoidK[Cocell]{
    override def empty[A]: Cocell[A] = ???

    override def combineK[A](x: Cocell[A], y: Cocell[A]): Cocell[A] = ???
  }

  def apply[A](implicit enc: Cocell[A]): Cocell[A] = enc
}

case class Context(database: String)

case class Fiber[A](value: A) extends Hom {
  val coalgebra: Coalgebra[Fiber, Fiber[A]] = Coalgebra(n => Fiber(n))

  val algebra: CVAlgebra[Fiber, Fiber[A]] = CVAlgebra {
    case _ => this
  }
}
