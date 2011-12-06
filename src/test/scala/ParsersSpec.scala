/**
 * scala-relaxng
 * For all details and documentation:
 * http://github.com/inkling/scala-relaxng
 *
 * Copyright 2011 Inkling Systems, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0

 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * 
 */

package com.inkling.relaxng.test

import com.inkling.relaxng._
import com.inkling.relaxng.AST._
import com.inkling.relaxng.ArbitraryInstances._
import com.inkling.relaxng.Parsers._
import com.inkling.relaxng.Pretty._

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.text.Document
import java.io.StringWriter

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.{Gen, Prop, Arbitrary}
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

class ParsersSpec extends Spec with Checkers {
  def show[T](v: T)(implicit p: Pretty[T]) : String = {
    val w = new StringWriter
    pretty(v).format(200, w)
    w.toString
  }

  implicit def scalacheckPretty[T](v: T)(implicit p: Pretty[T]) : org.scalacheck.Pretty = org.scalacheck.Pretty { params => show(v) }

  /** Little wrapper because I never "it" without a "check" */
  def checkit(description: String)(prop: Prop) { it(description) { check(prop) } }

  /** A simple wrapper so that exceptions do not prevent Prop labels from attaching */
  def noThrow[P](f: => P)(implicit p: P => Prop) : Prop = try { p(f) } catch { case _ => false }

  //def equal[T](expected: T, got: T)(implicit p : Pretty[T]) : Prop = expecte

  def parsePretty[T](gen: Gen[T])(parser: Parser[T])(implicit p: Pretty[T]) : Prop = 
    forAll(gen) { 
      v: T => 
        val result = parse(parser, show(v)).get
        "input = " + v |:
        "output = " + result |: 
        (v == result)
    }

  def parsePretty[T](parser: Parser[T])(implicit arb: Arbitrary[T], p: Pretty[T]) : Prop = parsePretty(arbitrary[T])(parser)

  describe("A RelaxNg (compact) parser") {
    describe("identity == (parse . pretty)") {
      checkit("unary operators")  { parsePretty(postfixUnOp | prefixUnOp) }
      checkit("binary operators") { parsePretty(binOp) }
      checkit("non-colon names") { parsePretty(ncName) }
      checkit("colon names") { parsePretty(cName) }
      checkit("literals") { parsePretty(literal) }
      checkit("datatype names") { parsePretty(datatypeName) }
      checkit("literal patterns") { parsePretty(literalPattern) }
      checkit("datatype parameters") { parsePretty(datatypeParams) }
      checkit("datatype patterns") { parsePretty(datatypePattern) }
      // Not quite yet... checkit("patterns") { parsePretty(pattern) }
    }
  }
}
