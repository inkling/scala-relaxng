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
 * A spec class to more fluidly support using spec and checkers together
 */
class CheckSpec extends Spec with Checkers {

  /** Little wrapper because I never "it" without a "check" */
  def checkit(description: String)(prop: =>Prop) { it(description) { check(prop) } }
}
