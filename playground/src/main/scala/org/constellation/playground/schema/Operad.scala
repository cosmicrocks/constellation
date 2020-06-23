package org.constellation.playground.schema

import cats.free.Free
import cats.kernel.Monoid
import cats.{Applicative, Eval, Functor, Inject, Monad, MonoidK, Traverse, ~>}
import higherkindness.droste.{CVAlgebra, Coalgebra}
import org.constellation.playground.schema.Enrichment.{A, M}
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, TypeClass}
import shapeless._

trait Operad {
  def tensor(x: Operad, y: Operad): Operad = this //https://ncatlab.org/nlab/show/Boardman-Vogt+tensor+product
  def endo(x: Operad)(transformation: Operad => Operad): Operad = endo(x)(transformation)
  def unit: Operad = this
}

object Operad {
  //todo use for lifts in Hom
  def ifEndo[A](g: A => A, pred: A => Boolean) : A => A = {
    a =>
      val newA = g(a)
      if (pred(newA)) newA else a
  }

  //todo use algebird in these transformations
  //when making higher order functions over Coproducts,
  // define like so https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#heterogenous-maps
  object size extends Poly1 {
    implicit def caseInt = at[Int](i => (i, i))
    implicit def caseString = at[String](s => (s, s.length))
    implicit def caseBoolean = at[Boolean](b => (b, 1))
  }
}

trait FreeOperad[A] extends Operad { self =>
  val data: A = self.unit.data

  override def tensor(x: Operad, y: Operad): Operad = tensor(x, y)

  override def endo(x: Operad)(transformation: Operad => Operad): Operad = endo(x)(transformation)

  override def unit: FreeOperad[A] = this

  def endo(x: FreeOperad[A])(transformation: FreeOperad[A] => FreeOperad[A]): FreeOperad[A] = transformation(x)

  def tensor(x: FreeOperad[_], y: FreeOperad[_] = this): FreeOperad[A] = this //todo implement
}

object FreeOperad extends TypeClass[FreeOperad] {
  type ISB = Operad

  override def coproduct[L, R <: Coproduct](cl: => FreeOperad[L], cr: => FreeOperad[R]): FreeOperad[L :+: R] = ???

  override def emptyCoproduct: FreeOperad[CNil] = ???

  override def product[H, T <: HList](ch: FreeOperad[H], ct: FreeOperad[T]): FreeOperad[H :: T] = ???

  override def emptyProduct: FreeOperad[HNil] = ???

  override def project[F, G](instance: => FreeOperad[G], to: F => G, from: G => F): FreeOperad[F] = ???

  implicit val freeOperadMonoid = new MonoidK[FreeOperad]{
    override def empty[A]: FreeOperad[A] = new FreeOperad[A] {}

    override def combineK[A](x: FreeOperad[A], y: FreeOperad[A]): FreeOperad[A] = x.tensor(y, x)
  }

  implicit val traverseInstance: Traverse[FreeOperad] = new Traverse[FreeOperad]{
    override def traverse[G[_], A, B](fa: FreeOperad[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[FreeOperad[B]] = ???

    override def foldLeft[A, B](fa: FreeOperad[A], b: B)(f: (B, A) => B): B = ???

    override def foldRight[A, B](fa: FreeOperad[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
  }
}

