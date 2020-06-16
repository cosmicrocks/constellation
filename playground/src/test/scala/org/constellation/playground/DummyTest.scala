package org.constellation.playground

import cats.data.Kleisli
import cats.effect.IO
import higherkindness.droste.data.Fix
import higherkindness.droste.{Algebra, Coalgebra, scheme}
import org.constellation.playground.schema.Cell.{Context}
import org.constellation.playground.schema.{Bundle, Cell, Compute, Fiber, Lookup, Result, Upsert}
import org.scalatest.{FreeSpec, Matchers}

class DummyTest extends FreeSpec with Matchers {
  val resultCoalgebra: Coalgebra[Cell, Fiber] = Coalgebra[Cell, Fiber](fiber => Result(fiber))
  val stringExecuteAlgebra: Algebra[Cell, Kleisli[IO, Context, String]] =
    Algebra {
      case Result(a) => Kleisli.pure("result")
      case Lookup(a) =>
        a.flatMap { s =>
          Cell.lookup[IO](s)
        }
      case Upsert(_)  => ???
      case Compute(_) => ???
    }
  val printAlgebra: Algebra[Cell, String] =
    Algebra[Cell, String] {
      case Result(a)  => s"result: ${a}"
      case Lookup(a)  => s"lookup: ${a}"
      case Upsert(a)  => s"upsert: ${a}"
      case Compute(a) => s"compute; ${a}"
    }

  val print = scheme.cata(printAlgebra)
  "foo" in {
    val execute = scheme.cata(stringExecuteAlgebra)

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
