package io.github.edadma.rdb_sjs

trait Tupleseq extends Seq[Tuple] {

  def header: Option[IndexedSeq[String]]

  def types: IndexedSeq[Type]

}

abstract class AbstractTupleseq extends Tupleseq {

  def apply(idx: Int): Tuple = iterator.drop(idx).next()

  def length: Int = iterator length

}
