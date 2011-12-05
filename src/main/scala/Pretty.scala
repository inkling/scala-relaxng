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

trait Pretty[T] {
  def pretty(v: T) : Document
}

object Pretty { 
  companion =>
    def pretty[T](v: T)(implicit p: Pretty[T]) : Document = p.pretty(v)

    def apply[T](f: T => Document) : Pretty[T] = new Pretty[T] {
      def pretty(v: T) = f(v)
    }

    implicit def prettyUnOp = Pretty { op: UnOp => text(op.raw) }
    implicit def prettyBinOp = Pretty { op: BinOp => text(op.raw) }
    implicit def prettyNameClass = Pretty { nc: NameClass => text("\"\"") }
    implicit def prettyNCName = Pretty { ncName: NCName => text(ncName.raw) }
    implicit def prettyCName : Pretty[CName] = Pretty { cName: CName => pretty(cName.prefix) :: text(":") :: pretty(cName.suffix) }

    implicit def prettyPattern : Pretty[Pattern] = new Pretty[Pattern] { 
      def pretty(p: Pattern) : Document = p match {
        case Constant(raw) => text(raw)
        //case Element(nameClass, pattern) =>  text("element") :: companion.pretty(nameClass) :: pretty(pattern)
      }
  }
    /*
    case class Attribute(name: NameClass, pattern: Pattern) extends Pattern
    case class ApplyBinOp(op: BinOp, left: Pattern, right: Pattern) extends Pattern
    case class ApplyUnOp(op: UnOp, pattern: Pattern) extends Pattern
    case class PatternIdentifier(name: Identifier) extends Pattern
  case class Parent(name: Identifier) extends Pattern
  case class DataType(name: Option[DataTypeName], value: String) extends Pattern
  case class ComplexDataType(name: DataTypeName, params: Map[Identifier, String], except: Pattern) extends Pattern
  case class ExternalRef(uri: URI, inherit: Boolean) extends Pattern
  case class Grammar(grammar: Seq[GrammarContent]) extends Pattern

* abstract class NameClass
  abstract class Name extends NameClass
  case class NsName(ns: NCName, except: NameClass) extends NameClass
  case class AnyName(ns: NCName, except: NameClass) extends NameClass
  case class OrName(left: NameClass, right: NameClass) extends NameClass
  
  case class Identifier(raw: String) extends Name
  case class CName(prefix: NCName, suffix: NCName) extends Name
*/
}
