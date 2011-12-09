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
import scala.util.matching.Regex

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.{Gen, Prop, Arbitrary}
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

class PrettySpec extends Spec with Checkers {

  /** Little wrapper because I never "it" without a "check" */
  def checkit(description: String)(prop: Prop) { it(description) { check(prop) } }

  /** Compresses multiple space characters to one and trim leading/trailing and those near parens
   * TODO: don't be so lenient on the pretty-printer */
  def compress(s: String) : String = replace("\\(\\s+".r, "(", replace("\\s+\\)".r, ")", replace("\\s+".r, " ", replace("\\s+}".r, "}", replace("\\{\\s+".r, "{", s))))).trim

  /** flip regex.replaceAllIn */
  def replace(regex: Regex, result: String, s: String) = regex.replaceAllIn(s, result)

  /** Test a list of samples */
  def checkSamples[T](samples: Seq[(String, T)])(implicit p: Pretty[T]) {
    samples.foreach { case (string, ast) => checkit(string) { compress(prettyString(ast)) ?= compress(string) } }
  }

  describe("A RelaxNg pretty-printer") {
    describe("prints canonical sample correctly modulo whitespace") { checkSamples(Samples.Canonical.nameClasses) }

    describe("prints canonical sample patterns correctly, modulo whitespace") { checkSamples(Samples.Canonical.patterns) }

    describe("prints canonical sample declarations correctly, modulo whitespace") {
      checkSamples(Samples.Canonical.declarations)
    }
    
    describe("prints canonical sample grammar content correctly, modulo whitespace") {
      checkSamples(Samples.Canonical.grammarContents)
    }

    describe("prints canonical sample schemas correctly, modulo whitespace") {
      checkSamples(Samples.Canonical.schemas)
    }
  }

}
