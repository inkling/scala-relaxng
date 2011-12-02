package com.inkling.relaxng

import AST._
  
import scala.util.parsing.combinator._
import scala.util.parsing.input._

// Via http://www.oasis-open.org/committees/relax-ng/compact-20020607.html

object Parsers extends RegexParsers with PackratParsers {

  def parse[T](p: Parser[T], s : String) : T = phrase(p)(new CharArrayReader(s.toArray)).get 

  def postfixUnOp : Parser[UnOp] = ("*" | "+") ^^ (UnOp(_))

  def prefixUnOp : Parser[UnOp] = ("list" | "mixed") ^^ (UnOp(_))

  def binOp : Parser[BinOp] = ("&" | "|" | ",") ^^ (BinOp(_))
}
