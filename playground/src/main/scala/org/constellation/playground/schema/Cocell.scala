package org.constellation.playground.schema

import cats.free.Cofree
import cats.{Eval, MonoidK}
import higherkindness.droste.{CVAlgebra, Coalgebra}

//todo, return State Objects from Cell.ghylo and chain over enriched monad, or fold over cofree product in coCell
case class Cocell[A](value: A, stateTransitionEval: Eval[Cell[Cofree[Cell, A]]]) {
  //todo use Coproducts to chain Cocell.plan instances https://underscore.io/blog/posts/2017/03/29/free-inject.html
  def plan: Cofree[Cell, A] = Cofree[Cell, A](value, stateTransitionEval)
}

object Cocell extends Hom {
  //Monad is Enriched/free cofree comonadic, enrichment ensures the Free Traversals in poset ordering, makes it State-full
  //category. Thus, we pass Kleisli of State across Top dimension, and Cofree/parallel in co dimension. Trick is to use
  //State to handle concurrent orderings at compile time using state.
  implicit val cellMonoid = new MonoidK[Cocell]{
    override def empty[A]: Cocell[A] = ???
    override def combineK[A](x: Cocell[A], y: Cocell[A]): Cocell[A] = ???
  }
}

case class Context(database: String)

case class Fiber[A](value: A) extends Hom {
  //todo somehow use Bundle's List[Fiber] with Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])
  val coalgebra: Coalgebra[Fiber, Fiber[A]] = Coalgebra(n => Fiber(n))
  val algebra: CVAlgebra[Fiber, Fiber[A]] = CVAlgebra {
    case _ => this
  }
}
