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

  def parse[T](p: Parser[T], s : String) : ParseResult[T] = phrase(p)(new CharArrayReader(s.toArray))
  def parseBlithely[T](p: Parser[T], s : String) : T = parse(p, s).get

  def postfixUnOp : Parser[UnOp] = ("*" | "+") ^^ (UnOp(_))

  def prefixUnOp : Parser[UnOp] = ("list" | "mixed") ^^ (UnOp(_))

  def binOp : Parser[BinOp] = ("&" | "|" | ",") ^^ (BinOp(_))

  def colonPrefix : Parser[NCName] = ncName <~ ":"
  
  def ncName : Parser[NCName] = "[a-zA-Z][a-zA-Z0-9]*".r ^^ (NCName(_)) // For now, just alphanumeric strings

  def cName : Parser[CName] = colonPrefix ~ ncName ^^ { case prefix ~ suffix => CName(prefix, suffix) }
  
  def identifier : Parser[Identifier] = "[a-zA-Z][a-zA-Z0-9]*".r ^^ Identifier.apply

  lazy val datatypeName : PackratParser[DataTypeName] = (
      ("string" | "token") ^^ PrimitiveDataType.apply
    | cName ^^ DataTypeCName.apply
  )

  lazy val literal : PackratParser[String] = regex("\"[^\"]*\"|'[^']*'".r) // TODO: improve this; currently just strings w/out escapes

  lazy val datatypeValue : PackratParser[String] = literal 

  lazy val anyNameClass : PackratParser[AnyNameClass] = (colonPrefix?) <~ "*" ^^ (AnyNameClass.apply _)

  lazy val nameClass : PackratParser[NameClass] = (
      anyNameClass 
    | anyNameClass ~ ("-" ~> nameClass) ^^ { case any ~ except => ExceptNameClass(any, except) }
    | (nameClass <~ "|") ~ nameClass ^^ { case ~(left, right) => OrNameClass(left, right) }
    | "(" ~> nameClass <~ ")"
  )

  
  lazy val datatypeParam : PackratParser[(Identifier, String)] = identifier ~ ("=" ~> literal) ^^ { case ident ~ value => (ident, value) }

  lazy val datatypeParams : PackratParser[Map[Identifier, String]] = rep(datatypeParam) ^^ { _.toMap }

  lazy val pattern : PackratParser[Pattern] = (
      ("text"  | "empty" | "notAllowed") ^^ (Constant.apply _)
    | ("element" ~> nameClass) ~ ("{" ~> pattern <~ "}") ^^ { case nc ~ p => Element(nc, p) }
    | ("attribute" ~> nameClass) ~ ("{" ~> pattern <~ "}") ^^ { case nc ~ p => Attribute(nc, p) }
    | pattern ~ binOp ~ pattern ^^ { case p1 ~ op ~ p2 => ApplyBinOp(op, p1, p2) }
    | prefixUnOp ~ pattern ^^ { case op ~ p => ApplyUnOp(op, p) }
    | pattern ~ postfixUnOp ^^ { case p ~ op => ApplyUnOp(op, p) }
    | identifier ^^ PatternIdentifier.apply
    | "parent" ~> identifier ^^ Parent.apply
    | (datatypeName?) ~ datatypeValue ^^ { case name ~ value => DataType(name, value) }
    | datatypeName ~ datatypeParams ~ (("-" ~> pattern)?) ^^ { case name ~ params ~ except => ComplexDataType(name, params, except.getOrElse(Constant("empty"))) }
    // TODO: external URI refs and grammar content
  )
}
