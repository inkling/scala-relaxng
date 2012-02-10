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
import Parsers._

import java.net.URI
import java.io.File

/**
 * Flattening of ASTs by following links. Currently supports only relative file references.
 */
object Flatten {

  // Don't change the current dir, or be explicit about relative URI, kthx
  private lazy val currentURI:URI = new URI("file://%s".format(System.getProperty("user.dir")))

  def flatten(schema: Schema) : Schema = flatten(schema, currentURI)
  def flatten(schema: Schema, relativeTo: URI) : Schema = 
    Schema(schema.decl, 
           schema.content match {
             case Left(pattern)  => Left(flatten(pattern, relativeTo = relativeTo))
             case Right(grammar) => Right(grammar.map(flatten(_, relativeTo = relativeTo)))
           })

  def flatten(grammar: GrammarContent) : GrammarContent = flatten(grammar, relativeTo = currentURI)
  def flatten(grammar: GrammarContent, relativeTo: URI) : GrammarContent = grammar match {
    case Define(name, op, pattern) => Define(name, op, flatten(pattern, relativeTo = relativeTo))
    case Div(moreGrammars) => Div(moreGrammars.map(flatten(_, relativeTo = relativeTo)))
    case Include(uri, inherit, include) => {
      val absoluteURI = relativeTo.resolve(new URI(uri.raw))
      if (absoluteURI.getScheme != "file")
        throw new Exception("Sorry scala-relaxng does not support non-file URIs yet; trying to follow %s".format(absoluteURI))
      else {
        val otherSchema = Parsers.load(new File(absoluteURI.getPath)) match {
          case Success(schema, _) => schema
          case Failure(msg, _)  => throw new Exception("Error while parsing %s: %s".format(absoluteURI, msg))
        }

        // TODO: propertly treat the namespaces!
        otherSchema.content match {
          case Left(pattern) => throw new Exception("include \"%s\" references file containing a pattern, not a grammar.".format(uri))
          case Right(contents) => Div(contents)
        }
      }
    }
  }

  def flatten(pattern: Pattern) : Pattern = flatten(pattern, relativeTo = currentURI)
  def flatten(pattern: Pattern, relativeTo: URI) : Pattern = pattern match {
    case Element(name, pattern) => Element(name, flatten(pattern))
    case Attribute(name, pattern) => Attribute(name, flatten(pattern))
    case ApplyBinOp(op, left, right) => ApplyBinOp(op, flatten(left), flatten(right, relativeTo = relativeTo))
    case ApplyUnOp(op, arg) => ApplyUnOp(op, arg)
    case Grammar(grammar) => Grammar(grammar.map(flatten(_, relativeTo = relativeTo)))
    case ExternalRef(uri, inherit) =>
      val absoluteURI = relativeTo.resolve(uri)
      if (absoluteURI.getScheme != "file")
        throw new Exception("Sorry scala-relaxng does not support non-file URIs yet; trying to follow %s".format(absoluteURI))
      else {
        val otherSchema = Parsers.load(new File(absoluteURI.getPath)) match {
          case Success(schema, _) => schema
          case Failure(msg, _) => throw new Exception("Error while parsing %s: %s".format(absoluteURI, msg))
        }

        // TODO: propertly treat the namespaces!
        otherSchema.content match {
          case Left(pattern) => pattern
          case Right(contents) => Grammar(contents)
        }
      }
    case other => other
  }
}

