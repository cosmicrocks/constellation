package org.constellation.playground.schema

import cats.effect.Concurrent
import cats.free.Cofree
import cats.{Eval, MonoidK}

case class CoCellT[F[_] : Concurrent, A](value: A, stateTransitionEval: Eval[Cell[Cofree[Cell, A]]])

case class Cocell[A](value: A, stateTransitionEval: Eval[Cell[Cofree[Cell, A]]]) extends FreeOperad[A] {
  val plan: Cofree[Cell, A] = Cofree[Cell, A](value, stateTransitionEval)

  override def product(x: FreeOperad[_], y: FreeOperad[_]): FreeOperad[_] = ???

  override def tensor(x: FreeOperad[_], y: FreeOperad[_]): FreeOperad[_] = ???

  override def endo: Operad = ???
}

object Cocell {
  implicit val cellMonoid = new MonoidK[Cocell]{
    override def empty[A]: Cocell[A] = ???

    override def combineK[A](x: Cocell[A], y: Cocell[A]): Cocell[A] = ???
  }
}
