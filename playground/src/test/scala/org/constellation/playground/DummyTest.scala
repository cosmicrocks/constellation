package org.constellation.playground

import cats.Applicative
import cats.data.{Const, Kleisli}
import cats.effect.IO
import cats.implicits._
import higherkindness.droste.data.Fix
import higherkindness.droste.{Algebra, Coalgebra, scheme}
import org.constellation.playground.schema.{
  Block,
  Cell,
  EdgeBundle,
  FoldableFromTraverse,
  FreeOperad,
  Signature,
  Snapshot,
  Transaction
}
import org.scalatest.{FreeSpec, Matchers}

class DummyTest extends FreeSpec with Matchers {
//  val resultCoalgebra: Coalgebra[Cell, Fiber] = Coalgebra[Cell, Fiber](fiber => Result(fiber))
//  val stringExecuteAlgebra: Algebra[Cell, Kleisli[IO, Context, String]] =
//    Algebra {
//      case Result(a) => Kleisli.pure("result")
//      case Lookup(a) =>
//        a.flatMap { s =>
//          Cell.lookup[IO](s)
//        }
//      case Upsert(_)  => ???
//      case Compute(_) => ???
//    }
//  val printAlgebra: Algebra[Cell, String] =
//    Algebra[Cell, String] {
//      case Result(a)  => s"result: ${a}"
//      case Lookup(a)  => s"lookup: ${a}"
//      case Upsert(a)  => s"upsert: ${a}"
//      case Compute(a) => s"compute; ${a}"
//    }
//
//  val print = scheme.cata(printAlgebra)

  "tx" in {
    import org.constellation.playground.schema.Hom._

    val txs1 = List(Transaction(Signature(1)), Transaction(Signature(2)))
    val txs2 = List(Transaction(Signature(3)), Transaction(Signature(4)))

    val block1 = Block(EdgeBundle(txs1))
    val block2 = Block(EdgeBundle(txs2))

    implicit val ListTraverse = new FoldableFromTraverse[List] {
      override def traverse[G[_], A, B](
        fa: List[A]
      )(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = fa match {
        case head :: tail => G.map2(f(head), traverse(tail)(f)) { case (a, b) => a :: b }
        case Nil          => G.pure(Nil)
      }
    }

//    val result = ListTraverse.traverse(List(1, 2, 3))(Option(_))

//    val aa = block
    val result = traversableInstance.traverse(block1)(a => Option(a))

//    val result = traversableInstance.traverse(block)(a => Option(a))
    println(block1)
    println(result)
  }

  "foo" in {
//    val execute = scheme.cata(stringExecuteAlgebra)
//
//    val fiber = new Bundle(Seq.empty)
//    val fixedCell: Fix[Cell] = Fix(Lookup(Fix(Lookup(Fix(Result(fiber))))))
//
//    // ---
//
//    val ctx: Context = Context("localdb")
//    val res = exe.apply(fixedCell).run(ctx).unsafeRunSync()
//    println(res)

    "a" shouldBe "a"
  }
}
