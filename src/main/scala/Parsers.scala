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

import scala.io.Source
import java.io.{File, FileReader, BufferedReader}  
import java.net.URI
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.collection.immutable._

/**
 * Parser for RelaxNg Compact Syntax.
 */
object Parsers extends RegexParsers with PackratParsers {

  def parse[T](p: Parser[T], s : String) : ParseResult[T] = phrase(p)(new CharArrayReader(s.toArray))

  /** Load a schema from a file */
  def load(fileName: File) : ParseResult[Schema] = {
    val input = new PagedSeqReader( PagedSeq.fromLines(for (line <- Source.fromFile(fileName).getLines)
                                                            yield line.split("#")(0)))

    parse(phrase(schema), new PackratReader(input))
  }

  def braces[T](p: PackratParser[T]) : PackratParser[T] = "{" ~> p <~ "}"
  def parens[T](p: PackratParser[T]) : PackratParser[T] = "(" ~> p <~ ")"

  lazy val postfixUnOp : PackratParser[UnOp] = ("*" | "+" | "?") ^^ UnOp.apply
  lazy val prefixUnOp : PackratParser[UnOp] = ("list" | "mixed") ^^ UnOp.apply
  lazy val binOp : PackratParser[BinOp] = ("&" | "|" | ",") ^^ BinOp.apply
  lazy val assignOp : PackratParser[AssignOp] = ("=" | "&=" | "|=") ^^ AssignOp.apply

  lazy val colonPrefix : PackratParser[NCName] = ncName <~ ":"
  
  lazy val ncName : PackratParser[NCName] = "[a-zA-Z_][-a-zA-Z0-9._]*".r ^^ NCName.apply // TODO: parse the spec

  lazy val cName : PackratParser[CName] = colonPrefix ~ ncName ^^ { case prefix ~ suffix => CName(prefix, suffix) }

  val keywords = Set("attribute", "default", "datatypes", "div", "element", "empty" , "external"
                     , "grammar", "include", "inherit", "list", "mixed", "namespace", "notAllowed"
                     , "parent", "start", "string", "text", "token")

  // TODO: escaped keywords
  lazy val identifier : PackratParser[NCName] = ncName flatMap { 
    nc => 
      if (keywords.contains(nc.raw))
        failure("keyword used as identifier")
      else
        success(nc)
  }

  lazy val identifierOrStart: PackratParser[NCName] = identifier | ("start" ^^ NCName.apply)
  
  lazy val identifierOrPrimitive: PackratParser[Pattern] = (
      identifier 
    | ("text" | "empty" | "notAllowed") ^^ NCName.apply
  ) ^^ NCNamePattern.apply

  lazy val keyword : PackratParser[NCName] = 
    ("attribute" | "default" | "datatypes" | "div" | "element" | "empty" | "external"
     | "grammar" | "include" | "inherit" | "list" | "mixed" | "namespace" | "notAllowed"
     | "parent" | "start" | "string" | "text" | "token") ^^ NCName.apply

  lazy val datatypeName : PackratParser[DatatypeName] = (
        ("string" | "token") ^^ PrimitiveDatatype.apply
    ||| cName ^^ DatatypeCName.apply
  )

  lazy val literal : PackratParser[Literal] = "\"" ~> regex("[^\"]*".r) <~ "\"" ^^ Literal.apply // TODO: improve this; currently just strings w/out escapes
  lazy val uri : PackratParser[URI] = literal ^^ { l => new URI(l.raw) }

  lazy val anyNameClass : PackratParser[WildcardNameClass] = (colonPrefix?) <~ "*" ^^ (WildcardNameClass.apply _)

  lazy val nameClass : PackratParser[NameClass] = (
      anyNameClass ~ ("-" ~> nameClass) ^^ { case any ~ except => ExceptNameClass(any, except) }
    | (nameClass <~ "|") ~ nameClass ^^ { case ~(left, right) => OrNameClass(left, right) }
    | "(" ~> nameClass <~ ")"
    | anyNameClass 
    | cName
    | identifier
  )

  lazy val datatypeParam : PackratParser[(NCName, Literal)] = identifier ~ ("=" ~> literal) ^^ { case ident ~ value => (ident, value) }

  lazy val datatypeParams : PackratParser[Map[NCName, Literal]] = (("{" ~> rep(datatypeParam) <~ "}")?) ^^ { case None    => Map[NCName, Literal]()
                                                                                                             case Some(l) => l.toMap }

  lazy val literalPattern : PackratParser[LiteralPattern] = (datatypeName?) ~ literal ^^ { case name ~ value => LiteralPattern(name, value) }
  lazy val datatypePattern : PackratParser[Datatype] = datatypeName ~ datatypeParams ^^ { case name ~ params => Datatype(name, params) }
  lazy val parent : PackratParser[Parent] = "parent" ~> identifier ^^ Parent.apply
  lazy val applyUnOp : PackratParser[ApplyUnOp] = (
      prefixUnOp ~ ("{" ~> pattern <~ "}") ^^ { case op ~ p => ApplyUnOp(op, p) }
    | pattern ~ postfixUnOp ^^ { case p ~ op => ApplyUnOp(op, p) }
  )
  
  lazy val applyBinOp : PackratParser[ApplyBinOp] = pattern ~ binOp ~ pattern ^^ { case p1 ~ op ~ p2 => ApplyBinOp(op, p1, p2) }

  lazy val element : PackratParser[Element] = ("element" ~> nameClass) ~ ("{" ~> pattern <~ "}") ^^ { case nc ~ p => Element(nc, p) }
    
  lazy val attribute : PackratParser[Attribute] =  ("attribute" ~> nameClass) ~ ("{" ~> pattern <~ "}") ^^ { case nc ~ p => Attribute(nc, p) }

  lazy val externalPattern : PackratParser[Pattern] = ("external" ~> literal) ~ (inherit?) ^^ { case uri ~ inherit => ExternalRef(new URI(uri.raw), inherit) }

  lazy val pattern : PackratParser[Pattern] = (
      applyUnOp
    | applyBinOp 
    | element
    | attribute
    | literalPattern
    | datatypePattern
    | parent
    | identifierOrPrimitive 
    | externalPattern
    | "(" ~> pattern <~ ")"
    // TODO: grammar content
  )

  lazy val namespaceValue : PackratParser[Either[URI, Inherit]] = (
      "inherit" ^^^ Right(Inherit())
    | literal ^^ { l => Left(new URI(l.raw)) }
  )

  lazy val declaration: PackratParser[Declaration] = (
      ("default" ~> "namespace" ~> (identifier?)) ~ ("=" ~> namespaceValue) ^^ { case ~(maybeIdent, v) => DefaultNamespace(maybeIdent, v) }
    | ("namespace" ~> identifier) ~ ("=" ~> namespaceValue) ^^ { case ~(ident, v) => Namespace(ident, v) }
    | ("datatypes" ~> identifier) ~ ("=" ~> literal) ^^ { case ~(ident, l) => Datatypes(ident, l) }
  )

  lazy val inherit: PackratParser[NCName] = "inherit"  ~> "=" ~> identifier

  lazy val grammarContent: PackratParser[GrammarContent] = (
      identifierOrStart ~ (assignOp ~ pattern) ^^ { case ~(name, ~(assign, pattern)) => Define(name, assign, pattern) }
    | "div" ~> braces(grammarContent*) ^^ Div.apply
    | ("include" ~> literal) ~ ( (inherit?) ~ (braces(grammarContent*)?) ) ^^ { case ~(uri, ~(maybeInherit, None))    => Include(uri, maybeInherit, Seq())
                                                                                case ~(uri, ~(maybeInherit, Some(l))) => Include(uri, maybeInherit, l) }
  )

  lazy val schemaBody : PackratParser[Either[Pattern, Seq[GrammarContent]]] = (
        (grammarContent*) ^^ Right.apply
    ||| pattern ^^ Left.apply
  )

  lazy val schema: PackratParser[Schema] = (declaration*) ~ schemaBody ^^ { case ~(decls, body) => Schema(decls, body) }
  
}
