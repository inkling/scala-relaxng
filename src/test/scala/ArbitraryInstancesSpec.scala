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

import com.inkling.relaxng.AST._
import com.inkling.relaxng.ArbitraryInstances._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

class ArbitraryInstancesSpec extends Spec with Checkers {
  def checkit(description: String)(prop: =>Prop) {
    it(description) {
      check(prop)
    }
  }

  def nonFailing[T](g: Gen[T]) : Prop = noneFailing(Seq(g))

  describe("The RelaxNg Compact Syntax Arbitrary instances") {
    describe("Should all be non-failing (checking for excess recursion)") {
      checkit("unary operators")  { nonFailing(arbitrary[UnOp]) }
      checkit("binary operators") { nonFailing(arbitrary[BinOp]) }
      checkit("non-colon names")  { nonFailing(arbitrary[NCName]) }
      checkit("colon names")      { nonFailing(arbitrary[CName]) }
      checkit("name classes")     { nonFailing(arbitrary[NameClass]) }
      checkit("patterns")         { nonFailing(arbitrary[Pattern]) }
      checkit("declarations")     { nonFailing(arbitrary[Declaration]) }
      checkit("grammar content")  { nonFailing(arbitrary[GrammarContent]) }
      checkit("schema")           { nonFailing(arbitrary[Schema]) }
    }
  }
}
