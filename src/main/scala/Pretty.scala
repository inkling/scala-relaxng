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
import java.io.StringWriter

/**
 * Type class for pretty-printing a value to scala.text.Document
 */
trait Pretty[T] {
  def pretty(v: T) : Document
}

/**
 * Pretty instances for classes in com.inkling.relaxng.AST
 *
 * These are broken down very finely to facilitate testing and debugging: most AST nodes have their own instance.
 */
object Pretty { 
    /** The toplevel function for the type class */
    def pretty[T](v: T)(implicit p: Pretty[T]) : Document = p.pretty(v)

    /** Helper shortcut for pretty-printing directly to a string */
    def prettyString[T](v: T)(implicit p: Pretty[T]) : String = {
      val w = new StringWriter
      pretty(v).format(200, w)
      w.toString
    }

    /** Factory method for creating Pretty instances */
    def apply[T](f: T => Document) : Pretty[T] = new Pretty[T] {
      def pretty(v: T) = f(v)
    }

    def parens(doc: Document) : Document = text("(") :: doc :: text(")")
    def braces(doc: Document) : Document = text("{") :: doc :: text("}")

    implicit def prettyUnOp = Pretty { op: UnOp => text(op.raw) }
    implicit def prettyBinOp = Pretty { op: BinOp => text(op.raw) }
    implicit def prettyNCName = Pretty { ncName: NCName => text(ncName.raw) }
    implicit def prettyCName : Pretty[CName] = Pretty { cName: CName => pretty(cName.prefix) :: text(":") :: pretty(cName.suffix) }
    implicit def prettyIdentifier : Pretty[Identifier] = Pretty { ident: Identifier => text(ident.raw) }
    implicit def prettyName : Pretty[Name] = Pretty { 
      case i:Identifier => pretty(i)
      case c:CName => pretty(c)
    }
    
    implicit def prettyAnyNameClass : Pretty[AnyNameClass] = Pretty { 
      case AnyNameClass(None) => text("*")
      case AnyNameClass(Some(ns)) => pretty(ns) :: text(":*")
    }

    implicit def prettyNameClass : Pretty[NameClass] = Pretty { 
      case OrNameClass(left, right) => parens(pretty(left) :/: text("|") :/: pretty(right))
      case ExceptNameClass(any, except) => parens(pretty(any) :/: text("- ") :: pretty(except))
      case any:AnyNameClass => pretty(any)
      case n:Name => prettyName.pretty(n)
    }

    def prefix(op: UnOp) : Boolean = op.raw match { case "list" | "mixed"  => true; case _ => false }
    def postfix(op: UnOp) : Boolean = op.raw match { case "*" | "+"  => true; case _ => false }

    implicit def prettyDatatypeName : Pretty[DatatypeName] = Pretty {
      case PrimitiveDatatype(raw) => text(raw)
      case DatatypeCName(name) => pretty(name)
    }

    implicit def prettyLiteral : Pretty[Literal] = Pretty { l => text("\"" + l.raw + "\"") } // TODO: escape properly, then adjust Arbitrary instance

    def prettyParam(key: Identifier, value: Literal) = pretty(key) :: text(" = ") :: pretty(value)
    implicit def prettyParams : Pretty[Map[Identifier, Literal]] = Pretty {
      params =>
        if (params.isEmpty) { empty }
        else braces {
          (for((key, value) <- params) yield prettyParam(key, value)).reduce(_ :/: _)
        }
    }

    implicit def prettyLiteralPattern : Pretty[LiteralPattern] = Pretty {
      case LiteralPattern(None, value) => pretty(value)
      case LiteralPattern(Some(name), value) => pretty(name) :/: pretty(value)
    }
      
    implicit def prettyDatatype : Pretty[Datatype] = Pretty { case Datatype(name, params) => pretty(name) :/: pretty(params) }
    implicit def prettyparent : Pretty[Parent] = Pretty { case Parent(ident) => text("parent") :/: pretty(ident) }
    implicit def prettyApplyUnOp : Pretty[ApplyUnOp] = Pretty { 
      case ApplyUnOp(op, p) if prefix(op) => parens(pretty(op) :/: braces(pretty(p)))
      case ApplyUnOp(op, p) if postfix(op) => parens(pretty(p) :/: pretty(op))
    }
      
    implicit def prettyPrimitivePattern : Pretty[PrimitivePattern] = Pretty { case PrimitivePattern(raw) => text(raw) }

    implicit def prettyPattern : Pretty[Pattern] = Pretty { 
      case Element(nameClass, pattern) =>  text("element") :/: pretty(nameClass) :/: text("{") :/: pretty(pattern) :/: text("}")
      case Attribute(nameClass, pattern) => text("attribute") :/: pretty(nameClass) :/: text("{") :/: pretty(pattern) :/: text("}")
      case ApplyBinOp(op, left, right) => parens(pretty(left) :/: pretty(op) :/: pretty(right))
      case PatternIdentifier(name) => pretty(name)
      case a:ApplyUnOp => pretty(a)
      case p:Parent => pretty(p)
      case p:PrimitivePattern => pretty(p)
      case l:LiteralPattern => pretty(l)
      case d:Datatype => pretty(d)
      // TODO: external refs and grammar content
    }
}
