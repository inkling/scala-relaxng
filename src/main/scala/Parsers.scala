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

/**
 * Parser for RelaxNg Compact Syntax.
 */
object Parsers extends RegexParsers with PackratParsers {

  def parse[T](p: Parser[T], s : String) : ParseResult[T] = p(new PackratReader(new CharArrayReader(s.toArray)))

  lazy val postfixUnOp : PackratParser[UnOp] = ("*" | "+") ^^ UnOp.apply

  lazy val prefixUnOp : PackratParser[UnOp] = ("list" | "mixed") ^^ UnOp.apply

  lazy val binOp : PackratParser[BinOp] = ("&" | "|" | ",") ^^ BinOp.apply

  lazy val colonPrefix : PackratParser[NCName] = ncName <~ ":"
  
  lazy val ncName : PackratParser[NCName] = "[a-zA-Z][a-zA-Z0-9]*".r ^^ NCName.apply // For now, just alphanumeric strings

  lazy val cName : PackratParser[CName] = colonPrefix ~ ncName ^^ { case prefix ~ suffix => CName(prefix, suffix) }

  lazy val identifier : PackratParser[Identifier] = not(keyword) ~> "[a-zA-Z][a-zA-Z0-9]*".r ^^ Identifier.apply
  
  lazy val keyword : PackratParser[String] = ("attribute" | "default" | "datatypes" | "div" | "element" | "empty" | "external"
                                              | "grammar" | "include" | "inherit" | "list" | "mixed" | "namespace" | "notAllowed"
                                              | "parent" | "start" | "string" | "text" | "token")

  lazy val datatypeName : PackratParser[DatatypeName] = (
      ("string" | "token") ^^ PrimitiveDatatype.apply
    | cName ^^ DatatypeCName.apply
  )

  lazy val literal : PackratParser[Literal] = "\"" ~> regex("[^\"]*".r) <~ "\"" ^^ Literal.apply // TODO: improve this; currently just strings w/out escapes

  lazy val anyNameClass : PackratParser[AnyNameClass] = (colonPrefix?) <~ "*" ^^ (AnyNameClass.apply _)

  lazy val nameClass : PackratParser[NameClass] = (
      anyNameClass 
    | cName
    | identifier
    | anyNameClass ~ ("-" ~> nameClass) ^^ { case any ~ except => ExceptNameClass(any, except) }
    | (nameClass <~ "|") ~ nameClass ^^ { case ~(left, right) => OrNameClass(left, right) }
    | "(" ~> nameClass <~ ")"
  )

  
  lazy val datatypeParam : PackratParser[(Identifier, Literal)] = identifier ~ ("=" ~> literal) ^^ { case ident ~ value => (ident, value) }

  lazy val datatypeParams : PackratParser[Map[Identifier, Literal]] = (("{" ~> rep(datatypeParam) <~ "}")?) ^^ { case None => Map[Identifier, Literal]()
                                                                                                                 case Some(l) => l.toMap }

  lazy val literalPattern : PackratParser[LiteralPattern] = (datatypeName?) ~ literal ^^ { case name ~ value => LiteralPattern(name, value) }
  lazy val primitivePattern : PackratParser[PrimitivePattern] = ("text" | "empty" | "notAllowed") ^^ PrimitivePattern.apply
  lazy val datatypePattern : PackratParser[Datatype] = datatypeName ~ datatypeParams ^^ { case name ~ params => Datatype(name, params) }
  lazy val parent : PackratParser[Parent] = "parent" ~> identifier ^^ Parent.apply
  lazy val applyUnOp : PackratParser[ApplyUnOp] = (
      prefixUnOp ~ ("{" ~> pattern <~ "}") ^^ { case op ~ p => ApplyUnOp(op, p) }
    | pattern ~ postfixUnOp ^^ { case p ~ op => ApplyUnOp(op, p) }
  )

  lazy val pattern : PackratParser[Pattern] = (
      primitivePattern
    | ("element" ~> nameClass) ~ ("{" ~> pattern <~ "}") ^^ { case nc ~ p => Element(nc, p) }
    | ("attribute" ~> nameClass) ~ ("{" ~> pattern <~ "}") ^^ { case nc ~ p => Attribute(nc, p) }
    | pattern ~ binOp ~ pattern ^^ { case p1 ~ op ~ p2 => ApplyBinOp(op, p1, p2) }
    | applyUnOp
    | literalPattern
    | datatypePattern
    | parent
    | identifier ^^ PatternIdentifier.apply
    | "(" ~> pattern <~ ")"
    // TODO: external URI refs and grammar content
  )
}
