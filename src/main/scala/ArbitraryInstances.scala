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

object ArbitraryInstances {
  implicit def arbUnOp : Arbitrary[UnOp] = Arbitrary(for(raw <- oneOf(Seq("*", "+", "list", "mixed"))) yield UnOp(raw))
  implicit def arbBinOp : Arbitrary[BinOp] = Arbitrary(for(raw <- oneOf(Seq("&", "|", ","))) yield BinOp(raw))
  implicit def arbAssignOp : Arbitrary[AssignOp] = Arbitrary(for(raw <- oneOf("=", "|=", "&=")) yield AssignOp(raw))

  implicit def arbDataTypeName : Arbitrary[DataTypeName] = Arbitrary(oneOf(resultOf(PrimitiveDataType),
                                                                           resultOf(DataTypeCName)))

  implicit def arbAnyNameClass : Arbitrary[AnyNameClass] = Arbitrary(resultOf(AnyNameClass))

  implicit def arbNameClass : Arbitrary[NameClass] = Arbitrary(frequency(2 -> arbitrary[Name],
                                                                         2 -> arbitrary[AnyNameClass],
                                                                         1 -> Gen.resultOf(ExceptNameClass),
                                                                         1 -> Gen.wrap(Gen.resultOf(OrNameClass))))

  implicit def arbIdentifier : Arbitrary[Identifier] = Arbitrary(identifier flatMap (Identifier(_)))
  implicit def arbNCName : Arbitrary[NCName] = Arbitrary(identifier flatMap (NCName(_)))
  implicit def arbCName : Arbitrary[CName] = Arbitrary(resultOf(CName))
  implicit def arbName : Arbitrary[Name] = Arbitrary(oneOf(arbitrary[Identifier], arbitrary[CName]))

  def genDataTypeValue : Gen[String] = Gen.alphaStr

  implicit def arbPattern : Arbitrary[Pattern] = Arbitrary(oneOf(oneOf("text", "empty", "notAllowed") flatMap(Constant.apply),
                                                                 wrap(resultOf(Element)),
                                                                 wrap(resultOf(Attribute)),
                                                                 wrap(resultOf(ApplyBinOp)),
                                                                 wrap(resultOf(ApplyUnOp)),
                                                                 wrap(resultOf(PatternIdentifier)),
                                                                 wrap(resultOf(Parent)),
                                                                 for(name <- arbitrary[Option[DataTypeName]]; value <- genDataTypeValue) yield DataType(name, value),
                                                                 wrap(resultOf(ComplexDataType))))
                            // TODO: external refs and grammar content

  implicit def arbURI : Arbitrary[URI] = Arbitrary(new URI("http://please/create/arbitrary/URI"))

  implicit def arbInherit : Arbitrary[Inherit] = Arbitrary(Inherit())
  
  implicit def arbDeclaration : Arbitrary[Declaration] = Arbitrary(Gen.oneOf(Gen.resultOf(Namespace),
                                                                             Gen.resultOf(DefaultNamespace),
                                                                             Gen.resultOf(Datatypes))) //TODO: limit values of Datatypes values

  implicit def arbGrammarContent : Arbitrary[GrammarContent] = Arbitrary(Gen.resultOf(Define))
                                                                                   // TODO: lists of grammar contentGen.resultOf(Div),
                                                                                   // TODO: lists of include contentGen.resultOf(Include)))
  
  implicit def arbSchema : Arbitrary[Schema] = Arbitrary(for(decls <- Gen.listOf(arbitrary[Declaration]);
                                                             content <- Gen.oneOf(arbitrary[Pattern] flatMap(Left(_)),
                                                                                  Gen.listOf(arbitrary[GrammarContent]) flatMap(Right(_))))
                                                         yield Schema(decls, content))
}
