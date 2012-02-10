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

import java.net.URI

import scala.text._
import scala.text.Document._
import java.io.StringWriter

/**
 * Type class for pretty-printing a value to scala.text.Document
 */
trait Pretty[T] {
  /** Pretty-printing */
  def pretty(v: T, inner: Boolean) : Document 
}

/**
 * Pretty instances for classes in com.inkling.relaxng.AST
 *
 * These are broken down very finely to facilitate testing and debugging: most AST nodes have their own instance.
 */
object Pretty { 
  /** The toplevel functions for the type class */
  def pretty[T](v: T, inner: Boolean)(implicit p: Pretty[T]) : Document = p.pretty(v, inner)
  def pretty[T](v: T)(implicit p: Pretty[T]) : Document = p.pretty(v, false)
  def innerPretty[T](v: T)(implicit p: Pretty[T]) : Document = p.pretty(v, true)

  /** Helper shortcut for pretty-printing directly to a string */
  def prettyString[T](v: T, width:Int = Int.MaxValue)(implicit p: Pretty[T]) : String = {
    val writer = new StringWriter
    pretty(v).format(width, writer)
    writer.toString
  }

  /** Factory method for creating Pretty instances where nesting does not matter */
  def simple[T](f: T => Document) : Pretty[T] = new Pretty[T] {
    def pretty(v: T, inner: Boolean) = f(v)
  }
  
  /** Factory method for creating Pretty instances that need to be aware of nesting */
  //def apply[T](f: (T, Boolean) => Document) : Pretty[T] = new Pretty[T] {
  //  def pretty(v: T, inner: Boolean) = f(v, inner)
  //}

  /** Curried factory method for reducing type annotations */
  def apply[T](f: Boolean => T => Document) : Pretty[T] = new Pretty[T] {
    def pretty(v: T, inner: Boolean) = f(inner)(v)
  }

  /** wraps a doc in "(" and ")" without breaks */
  def parens(doc: Document) : Document = group(text("(") :: doc :: text(")"))

  /** wraps a doc in "{" and "}" without breaks */
  def braces(doc: Document) : Document = text("{") :: doc :: text("}")

  /** wraps a doc in "{" and "}" with breaks and indentation */
  def block(doc: Document) : Document = group(text("{") :/: group(nest(2, doc)) :/: text("}"))

  /** wraps a doc in parens only if it is an inner expression */
  def innerParens(inner: Boolean) : Document => Document = if (inner) {d:Document => parens(d)} else {d:Document => d}

  /** Escapes a literal. TODO: make this robust and standards compliant */
  def escapeLiteral(s: String) : String = s.replace("\"","\\\"")

  /*
   * Instances
   */

  implicit def prettyUnOp : Pretty[UnOp] = simple { op => text(op.raw) }
  implicit def prettyBinOp : Pretty[BinOp] = simple { op => text(op.raw) }
  implicit def prettyAssignOp : Pretty[AssignOp] = simple { op => text(op.raw) }
  implicit def prettyNCName : Pretty[NCName] = simple { ncName => text(ncName.raw) }
  implicit def prettyCName : Pretty[CName] = simple { cName => pretty(cName.prefix) :: text(":") :: pretty(cName.suffix) }
  
  implicit def prettyWildcardNameClass : Pretty[WildcardNameClass] = simple { 
    case WildcardNameClass(None) => text("*")
    case WildcardNameClass(Some(ns)) => pretty(ns) :: text(":*")
  }

  implicit def prettyNameClass : Pretty[NameClass] = Pretty {
    inner => {
      case OrNameClass(left, right) => innerParens(inner)(innerPretty(left) :/: text("|") :/: innerPretty(right))
      case ExceptNameClass(any, except) => innerParens(inner)(innerPretty(any) :/: text("- ") :: innerPretty(except))
      case any:WildcardNameClass => pretty(any)
      case c:CName => pretty(c)
      case nc:NCName => pretty(nc)
    }
  }

  def prefix(op: UnOp) : Boolean = op.raw match { case "list" | "mixed"  => true; case _ => false }
  def postfix(op: UnOp) : Boolean = op.raw match { case "*" | "+" | "?" => true; case _ => false }
  
  implicit def prettyDatatypeName : Pretty[DatatypeName] = simple {
    dt: DatatypeName => dt match {
      case PrimitiveDatatype(raw) => text(raw)
      case DatatypeCName(name) => pretty(name)
    }
  }

  implicit def prettyLiteral : Pretty[Literal] = simple { l => text("\"" + escapeLiteral(l.raw) + "\"") } // TODO: escape properly, then adjust Arbitrary instance

  def prettyParam(key: NCName, value: Literal) = pretty(key) :: text(" = ") :: pretty(value)
  implicit def prettyParams : Pretty[Map[NCName, Literal]] = simple {
    params =>
      if (params.isEmpty) { empty }
      else braces {
        (for((key, value) <- params) yield prettyParam(key, value)).reduce(_ :/: _)
      }
  }

  implicit def prettyLiteralPattern : Pretty[LiteralPattern] = simple {
    case LiteralPattern(None, value) => pretty(value)
    case LiteralPattern(Some(name), value) => pretty(name) :/: pretty(value)
  }
  
  implicit def prettyDatatype : Pretty[Datatype] = simple { dt => dt match { case Datatype(name, params) => pretty(name) :/: pretty(params) } }
  implicit def prettyparent : Pretty[Parent] = simple { p => text("parent") :/: pretty(p.name) }
  implicit def prettyApplyUnOp : Pretty[ApplyUnOp] = Pretty { 
    inner => p => innerParens(inner) { p match {
      case ApplyUnOp(op, p) if prefix(op) => group( group(pretty(op) :/: text("{")) :/: pretty(p) :/: text("}") )
      case ApplyUnOp(op, p) if postfix(op) => innerPretty(p) :: pretty(op)
    }}
  }
  
  implicit def prettyPattern : Pretty[Pattern] = Pretty { 
    inner => {
      /* atoms; no needs for inner parens */
      case NCNamePattern(name) => pretty(name)
      case l:LiteralPattern => pretty(l)
      case d:Datatype => pretty(d, inner)
      case Element(nameClass, pattern) =>  text("element") :/: pretty(nameClass) :/: block(pretty(pattern))
      case Attribute(nameClass, pattern) => text("attribute") :/: pretty(nameClass) :/: block(pretty(pattern))
      case ExternalRef(uri, inherit) => (text("external") 
                                         :/: text("\"" + uri.toString + "\"") 
                                         :/: (inherit match { case None => empty; case Some(nc) => text("inherit = ") :: pretty(nc) }))
      case p => innerParens(inner) { p match {
        case ApplyBinOp(BinOp(","), left, right) => innerPretty(left) :: text(",") :/: innerPretty(right)
        case ApplyBinOp(op, left, right) => group(innerPretty(left) :/: pretty(op) :/: innerPretty(right))
        case a:ApplyUnOp => pretty(a)
        case p:Parent => pretty(p)
        // TODO: external refs and grammar content
      }}
    }
  }

  implicit def prettyUriOrInherit : Pretty[Either[URI, Inherit]] = simple {
    case Left(uri) => text("\"%s\"".format(escapeLiteral(uri.toString)))
    case Right(Inherit()) => text("inherit") 
  }

  implicit def prettyOptionNCName : Pretty[Option[NCName]] = simple {
    case None => empty
    case Some(nc) => pretty(nc)
  }
    
  implicit def prettyDeclaration : Pretty[Declaration] = simple {
    case Namespace(name, uriOrInherit) => text("namespace") :/: pretty(name) :/: text("=") :/: pretty(uriOrInherit)
    case DefaultNamespace(maybeName, uriOrInherit) => text("default namespace") :/: pretty(maybeName) :/: text("=") :/: pretty(uriOrInherit)
    case Datatypes(name, value) => text("datatypes") :/: pretty(name) :/: text("=") :/: pretty(value)
  }

  implicit def prettyGrammar : Pretty[Seq[GrammarContent]] = simple { g => if (g.size <= 0) empty 
                                                                           else braces(nest(2, group(g.map(pretty[GrammarContent]).reduce(_ :: text("\n") :/:_)))) }

  implicit def prettyGrammarContent : Pretty[GrammarContent] = simple {
    case Define(name, op, pattern) => pretty(name) :/: pretty(op) :/: pretty(pattern)
    case Div(grammar) => text("div") :/: (if (grammar.size > 0) pretty(grammar) else text("{ }"))
    case Include(uri, inherit, include) => (text("include") :/: pretty(uri) :/: 
                                            (inherit match { case None => empty; case Some(nc) => text("inherit =") :: pretty(nc) }) :/:
                                            pretty(include))
  }

  implicit def prettySchema : Pretty[Schema] = simple {
    s => (if (s.decl.size <= 0) empty else s.decl.map(pretty[Declaration]).reduce(_ :: text("\n") :/: _) :: text("\n")) :/: (s.content match {
      case Left(pattern) => pretty(pattern)
      case Right(grammar) => if (grammar.size <= 0) empty else grammar.map(pretty[GrammarContent]).reduce(_ :: text("\n") :/: _)
    })
  }
}
