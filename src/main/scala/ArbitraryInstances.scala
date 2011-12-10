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

import com.inkling.relaxng.AST._

import java.net.URI

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._

/**
 * ScalaCheck instances for classes in com.inkling.relaxng.AST
 *
 * For isolating test failures, most AST node classes have their own arbitrary instance.
 */
object ArbitraryInstances {
  /*
   * Atoms
   */

  /** Generates one of "*" "+" "list" and "mixed" */
  implicit def arbUnOp : Arbitrary[UnOp] = Arbitrary(for(raw <- oneOf(Seq("*", "+", "list", "mixed"))) yield UnOp(raw))

  /** Generates one of "&" "|" and "," */
  implicit def arbBinOp : Arbitrary[BinOp] = Arbitrary(for(raw <- oneOf(Seq("&", "|", ","))) yield BinOp(raw))

  /** Generates one of "=" "|=" and "&=" */
  implicit def arbAssignOp : Arbitrary[AssignOp] = Arbitrary(for(raw <- oneOf("=", "|=", "&=")) yield AssignOp(raw))

  /** Generates an arbitrary valid colon-free name (TODO; currently just [[org.scalacheck.Gen.identifier]]) */
  implicit def arbNCName : Arbitrary[NCName] = Arbitrary(for(head <- oneOf(alphaChar, '_');
                                                             tl <- listOf(oneOf(alphaChar, numChar, '-', '_', '.')))
                                                         yield NCName((head::tl).mkString))

  /** Generates an arbitrary valid colonized name i.e. <NCName>:<NCName> */
  implicit def arbCName : Arbitrary[CName] = Arbitrary(resultOf(CName))

  implicit def arbLiteral : Arbitrary[Literal] = Arbitrary(alphaStr.flatMap(Literal.apply))

  /** Generates one of "string" or "token" */
  implicit def arbPrimitiveDatatype : Arbitrary[PrimitiveDatatype] = Arbitrary(oneOf("string", "token") flatMap(PrimitiveDatatype.apply))

  /** Generates either a primitive datatype name or a colonized datatype name */
  implicit def arbDatatypeName : Arbitrary[DatatypeName] = Arbitrary(oneOf(arbitrary[PrimitiveDatatype], resultOf(DatatypeCName)))

  /** Generates a possibly-colonized "*" nameclass */
  implicit def arbWildcardNameClass : Arbitrary[WildcardNameClass] = Arbitrary(resultOf(WildcardNameClass))

  /*
   * Name classes
   */
  def leafNameClass : Gen[NameClass] = oneOf(arbitrary[NCName],
                                             arbitrary[CName],
                                             arbitrary[WildcardNameClass])

  /** Generates an ExceptNameClass using the subNameClass to generate the operands */
  def genExceptNameClass(subNameClass: Gen[NameClass]) : Gen[NameClass] =
    for(wildcard <- arbitrary[WildcardNameClass]; except <- subNameClass) yield ExceptNameClass(wildcard, except)

  /** Generates an OrNameClass using subNameClass to generate the operands */
  def genOrNameClass(subNameClass: Gen[NameClass]) : Gen[NameClass] = 
    for(left <- subNameClass; right <- subNameClass) yield OrNameClass(left, right)

  /** Generates a [[com.inkling.relaxng.AST.NameClass]] using subNameClass to generate subtrees.
   *  this may actually generate a leaf as well, with some probability */
  def recNameClass(subNameClass: Gen[NameClass]) : Gen[NameClass] = oneOf(leafNameClass, 
                                                                          genExceptNameClass(subNameClass), 
                                                                          genOrNameClass(subNameClass))
  
  /** Generates a name class of depth at most d */
  def nameClassOfDepth(d: Int) : Gen[NameClass] = if (d <= 0) leafNameClass
                                                  else recNameClass(nameClassOfDepth(d-1))

  /** Generates arbitrary name classes with syntactic depth <= 10 */
  implicit def arbNameClass : Arbitrary[NameClass] = Arbitrary(choose(0, 10) flatMap nameClassOfDepth)

  /*
   * Patterns
   */
  
  implicit def arbLiteralPattern : Arbitrary[LiteralPattern] = Arbitrary(resultOf(LiteralPattern))
  implicit def arbParent : Arbitrary[Parent] = Arbitrary(resultOf(Parent))
  implicit def arbDatatype : Arbitrary[Datatype] = Arbitrary(for(name <- arbitrary[DatatypeName];
                                                                 paramCount <- choose(0, 10);
                                                                 params <- listOfN(paramCount, arbitrary[(NCName, Literal)]))
                                                             yield Datatype(name, params.toMap))

  def leafPattern : Gen[Pattern] = oneOf(arbitrary[LiteralPattern],
                                         arbitrary[Parent],
                                         arbitrary[Datatype])
                            // TODO: external refs 

  /** Generates an element pattern using [[sub]] for the subpattern */
  def genElement(genNC: Gen[NameClass], sub: Gen[Pattern]) : Gen[Element] = 
    for(nc <- arbitrary[NameClass]; p <- sub) yield Element(nc, p)

  /** Generates an attribute pattern using [[sub]] for the subpattern */
  def genAttribute(genNC: Gen[NameClass], sub: Gen[Pattern]) : Gen[Attribute] = 
    for(nc <- genNC; p <- sub) yield Attribute(nc, p)

  /** Generates a unary operator application using [[sub]] for the operand */
  def genApplyUnOp(sub: Gen[Pattern]) : Gen[ApplyUnOp] = for(op <- arbitrary[UnOp]; p <- sub) yield ApplyUnOp(op, p)

  /** Generates a binary operator pattern using [[sub]] for the operands */
  def genApplyBinOp(sub: Gen[Pattern]) : Gen[ApplyBinOp] = for(op <- arbitrary[BinOp]; left <- sub; right <- sub) yield ApplyBinOp(op, left, right)

  def recursivePattern(nc: Gen[NameClass], subPattern: Gen[Pattern]) : Gen[Pattern] = oneOf(leafPattern,
                                                                                            genElement(nc, subPattern),
                                                                                            genAttribute(nc, subPattern),
                                                                                            genApplyUnOp(subPattern),
                                                                                            genApplyBinOp(subPattern))
    // TODO: grammar content

  /** Generates a pattern of depth at most d */
  def patternOfDepth(d: Int) : Gen[Pattern] = if (d <= 0) leafPattern
                                              else recursivePattern(nameClassOfDepth(d-1), patternOfDepth(d-1))
  
  /** Generates patterns up to depth 10 */
  implicit def arbPattern : Arbitrary[Pattern] = Arbitrary(choose(0, 10) flatMap patternOfDepth)

  implicit def arbURI : Arbitrary[URI] = Arbitrary(new URI("http://please/create/arbitrary/URI"))

  implicit def arbInherit : Arbitrary[Inherit] = Arbitrary(Inherit())
  
  /*
   * Declarations
   */
  implicit def arbDeclaration : Arbitrary[Declaration] = Arbitrary(Gen.oneOf(Gen.resultOf(Namespace),
                                                                             Gen.resultOf(DefaultNamespace),
                                                                             Gen.resultOf(Datatypes))) //TODO: limit values of Datatypes values
  /*
   * Grammar Content
   */
  implicit def arbGrammar : Arbitrary[Seq[GrammarContent]] = Arbitrary(for (n <- choose(0, 20); g <- listOfN(n, arbitrary[GrammarContent])) yield g)
  implicit def leafGrammarContent(pattern: Gen[Pattern]) : Gen[GrammarContent] = for(name <- arbitrary[NCName]; op <- arbitrary[AssignOp]; p <- pattern) yield Define(name, op, p)

  implicit def recursiveGrammarContent(pattern: Gen[Pattern], subGrammar:Gen[Seq[GrammarContent]]) : Gen[GrammarContent] = oneOf(leafGrammarContent(pattern),
                                                                                                                                 for (g <- subGrammar) yield Div(g),
                                                                                                                                 for (l <- arbitrary[Literal];
                                                                                                                                      n <- arbitrary[Option[NCName]];
                                                                                                                                      g <- subGrammar) yield Include(l, n, g))
  
  
  /** Generates grammar content of depth at most d */
  def grammarContentOfDepth(d: Int) : Gen[GrammarContent] = if (d <= 0) leafGrammarContent(patternOfDepth(0))
                                                            else recursiveGrammarContent(patternOfDepth(d-1), listOfN(d-1, grammarContentOfDepth(d-1)))

  /** Generates grammars of depth at most d */
  implicit def arbGrammarContent : Arbitrary[GrammarContent] = Arbitrary(choose(0, 10) flatMap grammarContentOfDepth)

  /*
   * Schema
   */
  implicit def arbSchema : Arbitrary[Schema] = Arbitrary(for(decls <- Gen.listOf(arbitrary[Declaration]);
                                                             content <- Gen.oneOf(arbitrary[Pattern] flatMap(Left(_)),
                                                                                  arbitrary[Seq[GrammarContent]] flatMap (Right(_))))
                                                         yield Schema(decls, content))
}

