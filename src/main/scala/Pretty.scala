package com.inkling.relaxng

import AST._

import scala.text._
import scala.text.Document._

trait Pretty[T] {
  def pretty(v: T) : Document
}

object Pretty {
  def pretty[T](v: T)(implicit p: Pretty[T]) : Document = p.pretty(v)

  implicit object prettyUnOp extends Pretty[UnOp] { def pretty(op: UnOp) : Document = text(op.raw) }
  implicit object prettyBinOp extends Pretty[BinOp] { def pretty(op: BinOp) : Document = text(op.raw) }
}
