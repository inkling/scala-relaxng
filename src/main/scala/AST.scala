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

import java.net.URI

/**  
 * See http://relaxng.org/compact-20021121.html for EBNF
 */
object AST {

  /**
   * A RelaxNg Compact Syntax Schema
   */
  case class Schema(decl: Seq[Declaration] = Seq(), content: Either[Pattern, Seq[GrammarContent]]) // topLevel

  /**
   * A schema may start with some namespaces or datatypes declarations before any grammar content
   */
  abstract class Declaration
  case class Namespace(name: NCName, uriOrInherit: Either[URI, Inherit]) extends Declaration // namespace <name> = (<literal> | inherit)
  case class DefaultNamespace(name: Option[NCName] = None, uriOrInherit: Either[URI, Inherit]) extends Declaration 
  case class Datatypes(name: NCName, value: Literal) extends Declaration
  case class Inherit() // Namespace constant
  
  /**
   * An item of grammar content is a definition (including "start"), a div, or an include
   */
  abstract class GrammarContent
  case class Define(name: NCName, op: AssignOp, pattern: Pattern) extends GrammarContent
  case class Div(grammar: Seq[GrammarContent]) extends GrammarContent
  case class Include(uri: Literal, inherit: Option[NCName], include: Seq[GrammarContent]) extends GrammarContent // Includes cannot be nested, but it just complicates the AST

  /**
   * A pattern matches XML content directly, either attributes or xml
   */
  abstract class Pattern
  case class Element(name: NameClass, pattern: Pattern) extends Pattern
  case class Attribute(name: NameClass, pattern: Pattern) extends Pattern
  case class ApplyBinOp(op: BinOp, left: Pattern, right: Pattern) extends Pattern
  case class ApplyUnOp(op: UnOp, pattern: Pattern) extends Pattern
  case class NCNamePattern(name: NCName) extends Pattern
  case class Parent(name: NCName) extends Pattern
  case class LiteralPattern(dataType: Option[DatatypeName], value: Literal) extends Pattern
  case class Datatype(name: DatatypeName, params: Map[NCName, Literal] = Map()) extends Pattern 
  /* case class ExceptPattern(datatype: Datatype, except: Pattern) // Never seen in the wild, but in the EBNF; TODO */
  case class ExternalRef(uri: URI, inherit: Option[NCName]) extends Pattern
  case class Grammar(grammar: Seq[GrammarContent]) extends Pattern

  implicit def datatypeToPattern(d: DatatypeName) : Datatype = Datatype(d)

  /**
   * A NameClass specifies a class of element names via using names, wildcards, union and difference operators
   */
  abstract class NameClass
  case class NCName(raw: String) extends NameClass
  case class WildcardNameClass(ns: Option[NCName] = None) extends NameClass // [<ns>:]*
  case class ExceptNameClass(any: WildcardNameClass, except: NameClass) extends NameClass // <any> - <except>
  case class OrNameClass(left: NameClass, right: NameClass) extends NameClass // <nameClass> | <nameClass>
  case class CName(prefix: NCName, suffix: NCName) extends NameClass

  /**
   * DatatypeName is either a colon-separated name or the built-in types "string" and "token"
   */
  abstract class DatatypeName
  case class PrimitiveDatatype(raw: String) extends DatatypeName // "string" | "token"
  case class DatatypeCName(name: CName) extends DatatypeName
  
  implicit def stringToPrimitiveDatatype(s:String) = PrimitiveDatatype(s)
  implicit def cnameToDatatypeName(c:CName) : DatatypeCName = DatatypeCName(c)

  case class AssignOp(raw: String) /** = |= &= */
  case class BinOp(raw: String) /** & | */
  case class UnOp(raw: String) /** * + list mixed */
  case class Literal(raw: String)
}
