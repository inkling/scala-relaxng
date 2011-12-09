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
import java.net.URI

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.{Gen, Prop, Arbitrary}
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

class ParsePrettySpec extends Spec with Checkers {

  /** Little wrapper because I never "it" without a "check" */
  def checkit(description: String)(prop: Prop) { it(description) { check(prop) } }

  def parsePretty[T](gen: Gen[T])(parser: Parser[T])(implicit p: Pretty[T]) : Prop = 
    forAll(gen) { 
      v: T => 
        try {
          val result = parse(parser, prettyString(v)).get
          "input = %s /// %s".format(prettyString(v), v) |:
          "output = %s /// %s".format(prettyString(result), result) |: 
          (v == result)
        } catch {
          case _ => "input failed to parse: %s (%s)".format(prettyString(v), v) |: falsified
          //case _:RuntimeException => falsified
        }
    }

  def parsePretty[T](parser: Parser[T])(implicit arb: Arbitrary[T], p: Pretty[T]) : Prop = parsePretty(arbitrary[T])(parser)

  describe("identity == (parse . pretty)") {
    describe("atoms") {
      checkit("unary operators")     { parsePretty(postfixUnOp | prefixUnOp) }
      checkit("binary operators")    { parsePretty(binOp) }
      checkit("non-colon names")     { parsePretty(ncName) }
      checkit("colon names")         { parsePretty(cName) }
      checkit("literals")            { parsePretty(literal) }
      checkit("datatype names")      { parsePretty(datatypeName) }
      checkit("identifiers")         { parsePretty(Parsers.identifier) }
      checkit("wildcard name class") { parsePretty(anyNameClass) }
    }

    describe("name classes") {
      for(i <- Range(0, 5)) {
        checkit("depth-%s name classes".format(i)) { parsePretty(nameClassOfDepth(i))(nameClass) }
      }
      checkit("arbitrary name classes")         { parsePretty(nameClass) }
    }

    describe("patterns") {
      checkit("primitive patterns")  { parsePretty(primitivePattern) }
      checkit("literal patterns")    { parsePretty(literalPattern) }
      checkit("datatype parameters") { parsePretty(datatypeParams) }
      checkit("datatype patterns")   { parsePretty(datatypePattern) }
      checkit("parent patterns")     { parsePretty(parent) }
      for(i <- Range(0, 5)) {
        checkit("depth-%s patterns".format(i)) { parsePretty(patternOfDepth(i))(pattern) }
      }
      // checkit("patterns") { parsePretty(pattern) }
    }
  }
}
