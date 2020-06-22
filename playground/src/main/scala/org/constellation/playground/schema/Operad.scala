package org.constellation.playground.schema

import cats.free.Free
import cats.kernel.Monoid
import cats.{Applicative, Eval, Functor, Inject, Monad, MonoidK, Traverse, ~>}
import org.constellation.playground.schema.Enrichment.{A, M}
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, TypeClass}
import shapeless._

trait Operad {
//todo we might want to make implicit Monoid out of this, to pick up type conversions
  def product(x: Operad, y: Operad): Operad

  //todo need for group action https://ncatlab.org/nlab/show/action and to show plan
  //allows for https://ncatlab.org/nlab/show/Day+convolution
  def tensor(x: Operad, y: Operad): Operad //https://ncatlab.org/nlab/show/Boardman-Vogt+tensor+product

  def endo: Operad
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

abstract class FreeOperad[A] extends Operad {
  override def product(x: Operad, y: Operad): Operad = product(x, y)
  override def tensor(x: Operad, y: Operad): Operad = tensor(x, y)
  def endo: Operad = ??? // todo return Hom
  def product(x: FreeOperad[_], y: FreeOperad[_]): FreeOperad[_]
  def tensor(x: FreeOperad[_], y: FreeOperad[_]): FreeOperad[_]
}

object FreeOperad extends TypeClass[FreeOperad] {
  //todo type ISB = FreeOperad :+: Hom <-> define coproducts. these should be imported when composing existing Cells

  implicit val traverseInstance: Traverse[FreeOperad] = new Traverse[FreeOperad]{
    override def traverse[G[_], A, B](fa: FreeOperad[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[FreeOperad[B]] = ???

    override def foldLeft[A, B](fa: FreeOperad[A], b: B)(f: (B, A) => B): B = ???

    override def foldRight[A, B](fa: FreeOperad[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
  }

  override def coproduct[L, R <: Coproduct](cl: => FreeOperad[L], cr: => FreeOperad[R]): FreeOperad[L :+: R] = ???

  override def emptyCoproduct: FreeOperad[CNil] = ???

  override def product[H, T <: HList](ch: FreeOperad[H], ct: FreeOperad[T]): FreeOperad[H :: T] = ???

  override def emptyProduct: FreeOperad[HNil] = ???

  override def project[F, G](instance: => FreeOperad[G], to: F => G, from: G => F): FreeOperad[F] = ???
}

//https://ncatlab.org/nlab/show/Day+convolution Proposition 3.3
abstract class Yoneda[F[_], A] {
  self =>
  def transformation[B](f: A => B): F[B]

  def run: F[A] = transformation(identity)

  def map[B](f: A => B): Yoneda[F, B] = new Yoneda[F, B] {
    def transformation[C](g: (B) => C): F[C] = self.transformation(g compose f)
  }
}

/**
  * Morphism object
  *
  * @param a
  * @param f todo make default where f is ghylo
  * @tparam X
  */
case class Morphism[X](a: A, f: A => M[X]) {
  def apply: M[X] = f(a)
}

object Enrichment {
  type A // What we start with
  type B // What we end up with
  type T[_] // Our Traversable collection type
  type M[_] // Our Monad target type
  type FreeMorphism[X] = Free[Morphism, X]

  def lift[X](a: A, func: A => M[X]): FreeMorphism[X] =
    Free.liftF(Morphism(a, func))

  val transformation = new (Morphism ~> M) {
    override def apply[X](o: Morphism[X]): M[X] = o.apply
  }

  def toYoneda[F[_], A](fa: F[A])(implicit F: Functor[F]): Yoneda[F, A] =
    new Yoneda[F, A] {
      override def transformation[B](f: (A) => B): F[B] = F.map(fa)(f)
    }

  def fromYoneda[F[_], A](lf: Yoneda[F, A]): F[A] = lf.run

  implicit class TopEnrichedTraverse[F[_], A](as: F[A]) {

    def topologicalTraverse[B, M[_]](f: A => M[B])(implicit T: Traverse[F], MonadM: Monad[M]): M[F[B]] = {
      case class LazyFunction[X](a: A, f: A => M[X]) {
        def apply: M[X] = f(a)
      }
      type FreeLazyFunction[X] = Free[LazyFunction, X]

      def lift[X](a: A, func: A => M[X]): FreeLazyFunction[X] =
        Free.liftF(LazyFunction(a, func))

      val transformation = new (LazyFunction ~> M) {
        override def apply[X](o: LazyFunction[X]): M[X] = o.apply
      }
      T.traverse(as)(lift(_, f)) //todo organize top within lift
        .foldMap(transformation)
    }
  }

}