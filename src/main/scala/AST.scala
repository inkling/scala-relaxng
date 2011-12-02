package com.inkling.relaxng

import java.net.URI

import org.scalacheck._

/**  
 * From http://www.oasis-open.org/committees/relax-ng/compact-20020607.html
 */
object AST {
  case class Schema(decl: Seq[Declaration], statement:Seq[Statement]) // Called "topLevel" in the grammar above

  case class Inherit() // A constant

  abstract class Declaration
  case class Namespace(name: Identifier, uriOrInherit: Either[URI, Inherit]) extends Declaration // name = uri
  case class DefaultNamespace(name: Option[Identifier], uriOrInherit: Either[URI, Inherit]) extends Declaration
  case class Datatypes(name: Identifier, value: String) extends Declaration
  
  abstract class Statement
  abstract class GrammarContent extends Statement
  abstract class Pattern extends Statement

  case class Start(op: AssignOp, pattern: Pattern) extends GrammarContent
  case class Define(name: Identifier, op: AssignOp, pattern: Pattern) extends GrammarContent
  case class Div(grammar: Seq[GrammarContent]) extends GrammarContent
  case class Include(uri: URI, inherit: Option[Identifier], include: Seq[GrammarContent]) extends GrammarContent // Includes cannot be nested, but it just complicates the AST

  case object Text extends Pattern
  case object Empty extends Pattern
  case object NotAllowed extends Pattern
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
  case class NsName(ns: NCName, except: NameClass) extends NameClass
  case class AnyName(ns: NCName, except: NameClass) extends NameClass
  case class OrName(left: NameClass, right: NameClass) extends NameClass
  
  case class Identifier(raw: String) extends Name
  case class CName(prefix: NCName, suffix: NCName) extends Name

  case class NCName(raw: String) // Non-colonized valid XML name matching regex [\i-[:]][\c-[:]]*

  abstract class DataTypeName
  case object StringDataType extends DataTypeName
  case object TokenDataType extends DataTypeName
  case class DataTypeCName(prefix: NCName, suffix: NCName) extends DataTypeName

  case class AssignOp(raw: String)
  case class BinOp(raw: String) // & | ,
  case class UnOp(raw: String) // * + list mixed

  implicit def arbUnOp : Arbitrary[UnOp] = Arbitrary(for(raw <- Gen.oneOf(Seq("*", "+", "list", "mixed"))) yield UnOp(raw))
  implicit def arbBinOp : Arbitrary[BinOp] = Arbitrary(for(raw <- Gen.oneOf(Seq("&", "|", ","))) yield BinOp(raw))
  implicit def arbAssignOp : Arbitrary[AssignOp] = Arbitrary(for(raw <- Gen.oneOf("=", "|=", "&=")) yield AssignOp(raw))
}
