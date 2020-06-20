package org.constellation.playground.schema

import cats.free.Free
import cats.{Functor, Inject, MonoidK}
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, TypeClass}
import shapeless._

//todo: We should organize:
// Snapshot <: CheckpointBlock <: Tx <: Hom and Hom can carry the monoids for forming edges. Note that this hierarchy
// is equivalent to:
// converged full state picture <: graph/hypergraph link <: linked list (topological ordering) <: user defined type class
trait Hom {
  //todo put edge/signing stuff here
}

trait Operad

abstract class FreeOperad[A] extends TypeClass[FreeOperad] {
  override def coproduct[L, R <: Coproduct](cl: => FreeOperad[L], cr: => FreeOperad[R]): FreeOperad[L :+: R] = ???

  override def emptyCoproduct: FreeOperad[CNil] = ???

  override def product[H, T <: HList](ch: FreeOperad[H], ct: FreeOperad[T]): FreeOperad[H :: T] = ???

  override def emptyProduct: FreeOperad[HNil] = ???

  override def project[F, G](instance: => FreeOperad[G], to: F => G, from: G => F): FreeOperad[F] = ???
}

object FreeOperad {
  def join[F[_] : Functor, A]: Free[F, Free[F, A]] => Free[F, A] = ???
}

abstract class Yoneda[F[_], A] {
  self =>
  def transformation[B](f: A => B): F[B]

  def run: F[A] = transformation(identity)

  def map[B](f: A => B): Yoneda[F, B] = new Yoneda[F, B] {
    def transformation[C](g: (B) => C): F[C] = self.transformation(g compose f)
  }
}