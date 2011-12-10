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

/**
 * A basic spec for parsers to ensure all fundamental constructs
 * and regressions are passing. See [[com.inkling.relaxng.test.ParsePretty]]
 * for a more thorough spec.
 */
class ParsersSpec extends CheckSpec {

  /** Test a list of samples */
  def checkSamples[T](parser: Parser[T], samples: Seq[(String, T)])(implicit p: Pretty[T]) {
    samples.foreach { case (string, ast) => checkit(string) { parseAll(parser, string).get ?= ast } }
  }

  def rejectSamples[T](parser: Parser[T], samples: Seq[String]) {
    samples.foreach { case (string) => checkit(string) { !parse(parser, string).successful } }
  }

  describe("A RelaxNg (compact) parser") {

    describe("parses sample name classes correctly") {
      checkSamples(nameClass, Samples.Canonical.nameClasses)
      checkSamples(nameClass, Samples.NonCanonical.nameClasses)
    }

    describe("parses sample datatype params correctly") {
      checkSamples(datatypeParams, Samples.Canonical.params)
    }
    
    describe("parses sample identifiers correctly") {
      checkSamples(Parsers.identifier, Samples.Canonical.identifiers)
    }

    describe("rejects bogus identifiers") {
      rejectSamples(Parsers.identifier, Samples.Rejects.identifiers)
    }
    
    describe("parses sample unary operator applications correctly") {
      checkSamples(pattern, Samples.Canonical.applyUnOps)
    }

    describe("parses sample patterns correctly") {
      checkSamples(pattern, Samples.Canonical.patterns)
      checkSamples(pattern, Samples.NonCanonical.patterns)
    }

    describe("parses sample declarations correctly") {
      checkSamples(declaration, Samples.Canonical.declarations)
      checkSamples(declaration, Samples.NonCanonical.declarations)
    }
    
    describe("parses sample grammar content correctly") {
      checkSamples(grammarContent, Samples.Canonical.grammarContents)
    }

    describe("parses sample schemas correctly") {
      checkSamples(schema, Samples.Canonical.schemas)
      checkSamples(schema, Samples.NonCanonical.schemas)
    }
  }
}
