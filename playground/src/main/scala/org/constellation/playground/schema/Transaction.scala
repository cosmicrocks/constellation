package org.constellation.playground.schema

import higherkindness.droste.{CVAlgebra, Coalgebra}

trait Edge {
  //mock for signatures
  def sign[A](data: A) = this
}

trait HyperEdge extends Edge {
  val fibers: Seq[Edge]
}

case object Signature extends Edge
case object EdgeData extends Edge
case class EdgeBundle(fibers: Seq[Edge]) extends HyperEdge

case class Transaction(data: Edge) extends Fiber[Edge] with Edge {
  def product(x: Hom[_], y: Hom[_]): Hom[_] = tensor(x, y)
  //todo need for group action https://ncatlab.org/nlab/show/action and to show plan
  def tensor(x: Hom[_], y: Hom[_]): Hom[_] = x //return Bundle[Edge], to be converted to HyperEdge
  def product(x: Transaction, y: Transaction): Hom[EdgeBundle] = Block(EdgeBundle(Seq(x.data) :+ y.data))
  def combine(x: Transaction, y: Transaction): Transaction = Transaction(x.data.sign(y.data))
  def newTxFromPrev(prev: Transaction) = combine(prev, this)

  override val coalgebra: Coalgebra[Hom, Edge] =  Coalgebra{
    case h: Hom[_] => Transaction(this.data.sign(h.data))
  }

  override val algebra: CVAlgebra[Hom, Edge] = CVAlgebra{
    case h: Hom[_] => Transaction(this.data.sign(h.data))
  }

  //todo define for endo, probably want Union type (direct sum)
  override def endo: Hom[Edge] = this// todo fold over product, return Bundle[Edge]

  override def unit: Hom[Edge] = combine(this, this)
}

case class Block(data: EdgeBundle) extends Bundle[EdgeBundle](data) with Edge {
  //todo define for creating mixed parent edges, test covariance
  override def product(x: Operad, y: Operad): Operad = y
  override def unit: Bundle[EdgeBundle] = combine(this, this)

  def combine(x: Block, y: Block): Block = Block(EdgeBundle(x.data.fibers ++ y.data.fibers))

  override def tensor(x: Operad, y: Operad): Operad = x

  override def endo: Hom[EdgeBundle] = this

  override val coalgebra: Coalgebra[Hom, EdgeBundle] =  Coalgebra{
    case t: Hom[_] => Block(EdgeBundle(this.data.fibers))
  }
  override val algebra: CVAlgebra[Hom, EdgeBundle] = CVAlgebra{
    case t: Hom[_] => EdgeBundle(this.data.fibers)
  }

  override def product(x: Hom[_], y: Hom[_]): Hom[_] = x //todo form and return Hyperedge

  override def tensor(x: Hom[_], y: Hom[_]): Hom[_] = y
}

case class Snapshot(convergedState: Seq[Block]) extends Simplex[EdgeBundle](convergedState) with Edge {
  override def product(x: Operad, y: Operad): Operad = x
  def combine(x: Snapshot, y: Snapshot): Snapshot = Snapshot(x.convergedState.head :: Nil)

  override val coalgebra: Coalgebra[Hom, EdgeBundle] =  Coalgebra{
    case t: Hom[_] => Block(this.convergedState.head.data)
  }
  override val algebra: CVAlgebra[Hom, EdgeBundle] = CVAlgebra{
    case t: Hom[_] => this.convergedState.head.data
  }
  override val data: EdgeBundle = this.convergedState.head.data

  override def product(x: Hom[_], y: Hom[_]): Hom[_] = x

  override def tensor(x: Hom[_], y: Hom[_]): Hom[_] = y

  override def endo: Hom[EdgeBundle] = this

  override def unit: Hom[EdgeBundle] = this
}


object ChannelApp extends App {
  import Cell._
  import Enrichment.TopEnrichedTraverse
  val channel = Cell[Transaction](Transaction(Signature))
}