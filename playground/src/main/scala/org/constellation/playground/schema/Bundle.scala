package org.constellation.playground.schema

//todo use Bundle.fibers for Traversable
class Bundle[A](fibers: Seq[Fiber[A]]) extends Hom

object Bundle {
  val example: Hom = new Bundle(Seq(new Fiber[String]("")))
}
