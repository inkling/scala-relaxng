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
  
import scala.util.parsing.combinator._
import scala.util.parsing.input._

// Via http://www.oasis-open.org/committees/relax-ng/compact-20020607.html

object Parsers extends RegexParsers with PackratParsers {

  def parse[T](p: Parser[T], s : String) : T = phrase(p)(new CharArrayReader(s.toArray)).get 

  def postfixUnOp : Parser[UnOp] = ("*" | "+") ^^ (UnOp(_))

  def prefixUnOp : Parser[UnOp] = ("list" | "mixed") ^^ (UnOp(_))

  def binOp : Parser[BinOp] = ("&" | "|" | ",") ^^ (BinOp(_))
}
