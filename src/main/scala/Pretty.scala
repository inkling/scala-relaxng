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
