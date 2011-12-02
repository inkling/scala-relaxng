package com.inkling.relaxng.test

import com.inkling.relaxng._
import com.inkling.relaxng.AST._
import com.inkling.relaxng.Parsers._
import com.inkling.relaxng.Pretty._

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.text.Document
import java.io.StringWriter

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

class ParsersSpec extends Spec with Checkers {
  def string(d: Document) : String = {
    val w = new StringWriter
    d.format(200, w)
    w.toString
  }

  def checkit(description: String)(prop: Prop) {
    it(description) {
      check(prop)
    }
  }

  def parsePretty[T](gen: Gen[T], parser: Parser[T])(implicit p: Pretty[T]) : Prop =
    forAll(gen) { v: T => v == parse(parser, string(pretty(v))) }

  describe("A RelaxNg (compact) parser") {
    describe("identity == (parse . pretty)") {
      checkit("unary operators")  { parsePretty(arbitrary[UnOp], postfixUnOp | prefixUnOp) }
      checkit("binary operators") { parsePretty(arbitrary[BinOp], binOp) }
    }
  }
}
