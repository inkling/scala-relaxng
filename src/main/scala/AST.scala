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

import org.scalacheck._
import org.scalacheck.Arbitrary._

/**  
 * See http://relaxng.org/compact-20021121.html for EBNF
 */
object AST {
  case class Schema(decl: Seq[Declaration], content: Either[Pattern, Seq[GrammarContent]]) // topLevel

  case class Inherit() // A constant

  abstract class Declaration
  case class Namespace(name: Identifier, uriOrInherit: Either[URI, Inherit]) extends Declaration // name = uri
  case class DefaultNamespace(name: Option[Identifier], uriOrInherit: Either[URI, Inherit]) extends Declaration
  case class Datatypes(name: Identifier, value: String) extends Declaration
  
  abstract class GrammarContent
  case class Start(op: AssignOp, pattern: Pattern) extends GrammarContent
  case class Define(name: Identifier, op: AssignOp, pattern: Pattern) extends GrammarContent
  case class Div(grammar: Seq[GrammarContent]) extends GrammarContent
  case class Include(uri: URI, inherit: Option[Identifier], include: Seq[GrammarContent]) extends GrammarContent // Includes cannot be nested, but it just complicates the AST

  abstract class Pattern
  case class Constant(raw: String) extends Pattern // text | empty | notAllowed
  case class Element(name: NameClass, pattern: Pattern) extends Pattern
  case class Attribute(name: NameClass, pattern: Pattern) extends Pattern
  case class ApplyBinOp(op: BinOp, left: Pattern, right: Pattern) extends Pattern
  case class ApplyUnOp(op: UnOp, pattern: Pattern) extends Pattern
  case class PatternIdentifier(name: Identifier) extends Pattern
  case class Parent(name: Identifier) extends Pattern
  case class DataType(name: Option[DataTypeName], value: String) extends Pattern
  case class ComplexDataType(name: DataTypeName, params: Map[Identifier, String], except: Pattern) extends Pattern
  case class ExternalRef(uri: URI, inherit: Boolean) extends Pattern
  case class Grammar(grammar: Seq[GrammarContent]) extends Pattern

  abstract class NameClass
  abstract class Name extends NameClass
  case class AnyNameClass(ns: Option[NCName]) extends NameClass // [<ns>:]*
  case class ExceptNameClass(any: AnyNameClass, except: NameClass) extends NameClass // <any> - <except>
  case class OrNameClass(left: NameClass, right: NameClass) extends NameClass // <nameClass> | <nameClass>
  
  case class Identifier(raw: String) extends Name
  case class CName(prefix: NCName, suffix: NCName) extends Name

  case class NCName(raw: String) // See http://www.w3.org/TR/REC-xml-names/ for actual allowed strings

  abstract class DataTypeName
  case class PrimitiveDataType(raw: String) extends DataTypeName
  case class DataTypeCName(name: CName) extends DataTypeName

  case class AssignOp(raw: String)
  case class BinOp(raw: String) // & | ,
  case class UnOp(raw: String) // * + list mixed

  implicit def arbUnOp : Arbitrary[UnOp] = Arbitrary(for(raw <- Gen.oneOf(Seq("*", "+", "list", "mixed"))) yield UnOp(raw))
  implicit def arbBinOp : Arbitrary[BinOp] = Arbitrary(for(raw <- Gen.oneOf(Seq("&", "|", ","))) yield BinOp(raw))
  implicit def arbAssignOp : Arbitrary[AssignOp] = Arbitrary(for(raw <- Gen.oneOf("=", "|=", "&=")) yield AssignOp(raw))

  implicit def arbDataTypeName : Arbitrary[DataTypeName] = Arbitrary(Gen.oneOf(Gen.resultOf(PrimitiveDataType),
                                                                               Gen.resultOf(DataTypeCName)))

  implicit def arbAnyNameClass : Arbitrary[AnyNameClass] = Arbitrary(Gen.resultOf(AnyNameClass))

  implicit def arbNameClass : Arbitrary[NameClass] = Arbitrary(Gen.oneOf(arbitrary[Name],
                                                                         arbitrary[AnyNameClass],
                                                                         Gen.resultOf(ExceptNameClass),
                                                                         Gen.resultOf(OrNameClass)))

  implicit def arbIdentifier : Arbitrary[Identifier] = Arbitrary(Gen.identifier flatMap (Identifier(_)))
  implicit def arbNCName : Arbitrary[NCName] = Arbitrary(Gen.identifier flatMap (NCName(_)))
  implicit def arbCName : Arbitrary[CName] = Arbitrary(Gen.resultOf(CName))
  implicit def arbName : Arbitrary[Name] = Arbitrary(Gen.oneOf(arbitrary[Identifier], arbitrary[CName]))

  def genDataTypeValue : Gen[String] = Gen.alphaStr

  implicit def arbPattern : Arbitrary[Pattern] = Arbitrary(Gen.oneOf(Gen.oneOf("text", "empty", "notAllowed") flatMap(Constant.apply),
                                                                     Gen.resultOf(Element),
                                                                     Gen.resultOf(Attribute),
                                                                     Gen.resultOf(ApplyBinOp),
                                                                     Gen.resultOf(ApplyUnOp),
                                                                     Gen.resultOf(PatternIdentifier),
                                                                     Gen.resultOf(Parent),
                                                                     for(name <- arbitrary[Option[DataTypeName]]; value <- genDataTypeValue) yield DataType(name, value),
                                                                     Gen.resultOf(ComplexDataType)))
                                                                     // TODO: external refs and grammar content

  implicit def arbURI : Arbitrary[URI] = Arbitrary(new URI("http://please/create/arbitrary/URI"))

  implicit def arbInherit : Arbitrary[Inherit] = Arbitrary(Inherit())
  
  implicit def arbDeclaration : Arbitrary[Declaration] = Arbitrary(Gen.oneOf(Gen.resultOf(Namespace),
                                                                             Gen.resultOf(DefaultNamespace),
                                                                             Gen.resultOf(Datatypes))) //TODO: limit values of Datatypes values

  implicit def arbGrammarContent : Arbitrary[GrammarContent] = Arbitrary(Gen.oneOf(Gen.resultOf(Start),
                                                                                   Gen.resultOf(Define)))
                                                                                   // TODO: lists of grammar contentGen.resultOf(Div),
                                                                                   // TODO: lists of include contentGen.resultOf(Include)))
  
  implicit def arbSchema : Arbitrary[Schema] = Arbitrary(for(decls <- Gen.listOf(arbitrary[Declaration]);
                                                             content <- Gen.oneOf(arbitrary[Pattern] flatMap(Left(_)),
                                                                                  Gen.listOf(arbitrary[GrammarContent]) flatMap(Right(_))))
                                                         yield Schema(decls, content))
}
