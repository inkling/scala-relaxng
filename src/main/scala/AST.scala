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
  case class Schema(decl: Seq[Declaration], content: Either[Pattern, Seq[GrammarContent]]) // topLevel

  /**
   * A schema may start with some namespaces or datatypes declarations before any grammar content
   */
  abstract class Declaration
  case class Namespace(name: Identifier, uriOrInherit: Either[URI, Inherit]) extends Declaration // namespace <name> = (<literal> | inherit)
  case class DefaultNamespace(name: Option[Identifier], uriOrInherit: Either[URI, Inherit]) extends Declaration 
  case class Datatypes(name: Identifier, value: Literal) extends Declaration
  case class Inherit() // Namespace constant
  
  /**
   * An item of grammar content is a definition (including "start"), a div, or an include
   */
  abstract class GrammarContent
  case class Define(name: Identifier, op: AssignOp, pattern: Pattern) extends GrammarContent
  case class Div(grammar: Seq[GrammarContent]) extends GrammarContent
  case class Include(uri: URI, inherit: Option[Identifier], include: Seq[GrammarContent]) extends GrammarContent // Includes cannot be nested, but it just complicates the AST

  /**
   * A pattern matches XML content directly, either attributes or xml
   */
  abstract class Pattern
  case class PrimitivePattern(raw: String) extends Pattern // text | empty | notAllowed
  case class Element(name: NameClass, pattern: Pattern) extends Pattern
  case class Attribute(name: NameClass, pattern: Pattern) extends Pattern
  case class ApplyBinOp(op: BinOp, left: Pattern, right: Pattern) extends Pattern
  case class ApplyUnOp(op: UnOp, pattern: Pattern) extends Pattern
  case class PatternIdentifier(name: Identifier) extends Pattern
  case class Parent(name: Identifier) extends Pattern
  case class LiteralPattern(dataType: Option[DatatypeName], value: Literal) extends Pattern
  case class Datatype(name: DatatypeName, params: Map[Identifier, Literal]) extends Pattern // Grammar refers to exceptPattern but we don't use it and they don't either in their tutorial
  case class ExternalRef(uri: URI, inherit: Boolean) extends Pattern
  case class Grammar(grammar: Seq[GrammarContent]) extends Pattern

  /**
   * A NameClass specifies a class of element names via using names, wildcards, union and difference operators
   */
  abstract class NameClass
  case class AnyNameClass(ns: Option[NCName]) extends NameClass // [<ns>:]*
  case class ExceptNameClass(any: AnyNameClass, except: NameClass) extends NameClass // <any> - <except>
  case class OrNameClass(left: NameClass, right: NameClass) extends NameClass // <nameClass> | <nameClass>
  
  /**
   * A Name is an identifier or colon-separated name
   */
  abstract class Name extends NameClass
  case class Identifier(raw: String) extends Name
  case class CName(prefix: NCName, suffix: NCName) extends Name
  case class NCName(raw: String) // See http://www.w3.org/TR/REC-xml-names/ for actual allowed strings

  /**
   * DatatypeName is either a colon-separated name or the built-in types "string" and "token"
   */
  abstract class DatatypeName
  case class PrimitiveDatatype(raw: String) extends DatatypeName // "string" | "token"
  case class DatatypeCName(name: CName) extends DatatypeName

  case class AssignOp(raw: String) /** = |= &= */
  case class BinOp(raw: String) /** & | */
  case class UnOp(raw: String) /** * + list mixed */
  case class Literal(raw: String)
}
