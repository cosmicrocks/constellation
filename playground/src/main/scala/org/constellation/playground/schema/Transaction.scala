package org.constellation.playground.schema

import higherkindness.droste.{CVAlgebra, Coalgebra}

trait Edge{
  //mock for signatures
  def sign(data: Edge) = data
}

trait HyperEdge extends Edge {
  val fibers: Seq[Edge]
}

case object Signature extends Edge
case object EdgeData extends Edge
case class EdgeBundle(fibers: Seq[Edge]) extends HyperEdge

case class Transaction(data: Edge) extends Fiber[Transaction] with Edge {
//todo define for endo, probably want Union type (direct sum)
  override def product(x: Operad, y: Operad): Operad = ???
  def combine(x: Transaction, y: Transaction): Transaction = Transaction(x.data.sign(y.data))
  def newTxFromPrev(prev: Transaction) = combine(prev, this)
  def getHash: Edge = new Edge {}

  override def unit: Fiber[Transaction] = combine(this, this)
}

case class Block(data: EdgeBundle) extends Bundle[EdgeBundle](data) with Edge {
  //todo define for creating mixed parent edges, test covariance
  override def product(x: Operad, y: Operad): Operad = ???
  override def unit: Bundle[EdgeBundle] = combine(this, this)

  def combine(x: Block, y: Block): Block = Block(EdgeBundle(x.data.fibers ++ y.data.fibers))
  def getHash: Edge = new Edge {}
}
//todo define for creating mixed parent edges across convergence criteria
case class Snapshot(data: Seq[Block]) extends Simplex[EdgeBundle](data) with Edge {
  override def product(x: Operad, y: Operad): Operad = ???
//  def combine(x: Snapshot, y: Snapshot): Snapshot = Snapshot(x.data.parents ++ y.data)
//  def id(prev: Snapshot) = combine(prev, this)//todo for flattening action chains
  def getHash: Edge = new Edge {}

  override def unit: Hom[Fiber[EdgeBundle]] = ???
}


object ChannelApp extends App {
  import Cell._
  import Enrichment.TopEnrichedTraverse
  val channel = Cell[Transaction](Transaction(Signature))
}