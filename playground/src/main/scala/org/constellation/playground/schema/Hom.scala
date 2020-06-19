package org.constellation.playground.schema

import cats.free.Free
import cats.{Functor, Inject}

//todo: We should organize:
// Snapshot <: CheckpointBlock <: Tx <: Hom and Hom can carry the monoids for forming edges. Note that this hierarchy
// is equivalent to:
// converged full state picture <: graph/hypergraph link <: linked list (topological ordering) <: user defined type class
trait Hom {
  //todo put edge/signing stuff here
}

trait Operad

class FreeOperad[C[_]](implicit inject: Inject[Operad, C[_]]) {}

object FreeOperad { //todo look into generics https://books.underscore.io/shapeless-guide/shapeless-guide.html#sec:generic:coproducts
  def join[F[_] : Functor, A]: Free[F, Free[F, A]] => Free[F, A] = ???

  implicit def injectCoproductLeft[F[_], X[_]] = ???

  implicit def injectCoproductRight[F[_], R[_], X[_]](implicit I: Inject[F[_], R[_]]) = ???

  implicit def injectReflexive[F[_]]: Inject[F[_], F[_]] = new Inject[F[_], F[_]] {
    def inj[A](fa: F[A]): F[A] = fa

    override def inj: F[_] => F[_] = ???

    override def prj: F[_] => Option[F[_]] = ???
  }
}

abstract class Yoneda[F[_], A] {
  self =>
  def transformation[B](f: A => B): F[B]

  def run: F[A] = transformation(identity)

  def map[B](f: A => B): Yoneda[F, B] = new Yoneda[F, B] {
    def transformation[C](g: (B) => C): F[C] = self.transformation(g compose f)
  }
}