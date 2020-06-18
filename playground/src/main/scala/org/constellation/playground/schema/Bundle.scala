package org.constellation.playground.schema

//todo use Bundle as Traversable
class Bundle(fibers: Seq[Fiber]) extends Fiber

object Bundle {
  val example: Fiber = new Bundle(Seq(new Bundle(Seq.empty)))
}
