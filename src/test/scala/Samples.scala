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

package com.inkling.relaxng.test

import com.inkling.relaxng.AST._

import java.net.URI

/**
 * A collection of sample (String, AST) pairs and regression tests
 * to supplement the fuzz tests in [[com.inkling.relaxng.test.ParsePretty]]
 */
object Samples {

  /**
   * Samples where the string is exactly what the pretty-printer should print, without
   * weird whitespace or parens.
   */
  object Canonical {
    val nameClasses = Seq[(String, NameClass)]("*"                   -> WildcardNameClass(),
                                               "foo:*"               -> WildcardNameClass(Some(NCName("foo"))),
                                               "foo"                 -> NCName("foo"),
                                               "* | *"               -> OrNameClass(WildcardNameClass(), WildcardNameClass()),
                                               "* - *"               -> ExceptNameClass(WildcardNameClass(), WildcardNameClass()),
                                               "foo:* - (x | y)"     -> ExceptNameClass(WildcardNameClass(Some(NCName("foo"))), OrNameClass(NCName("x"),
                                                                                                                                            NCName("y"))))
    
    val params = Seq[(String, Map[NCName, Literal])]("{ foo = \"bar\" }" -> Map(NCName("foo") -> Literal("bar")),
                                                     "{ foo = \"bar\" foo2 = \"bar2\"}" -> Map(NCName("foo") -> Literal("bar"),
                                                                                               NCName("foo2") -> Literal("bar2")),
                                                     "{ foo= \"bar\" }" -> Map(NCName("foo") -> Literal("bar")))

    val patterns = Seq[(String, Pattern)]("empty"                          -> PrimitivePattern("empty"),
                                          "bizzle"                         -> NCNamePattern(NCName("bizzle")),
                                          "element foo { empty }"          -> Element(NCName("foo"), PrimitivePattern("empty")),
                                          "attribute foo { empty }"        -> Attribute(NCName("foo"), PrimitivePattern("empty")),
                                          "attribute foo { notAllowed }"   -> Attribute(NCName("foo"), PrimitivePattern("notAllowed")),
                                          "string +"                       -> ApplyUnOp(UnOp("+"), PrimitiveDatatype("string")),
                                          "string *"                       -> ApplyUnOp(UnOp("*"), PrimitiveDatatype("string")),
                                          "xsd:bif { x = \"y\"}"           -> Datatype(CName(NCName("xsd"), NCName("bif")), Map(NCName("x") -> Literal("y"))),
                                          "s9:baf { z = \"w\" a = \"bc\"}" -> Datatype(CName(NCName("s9"), NCName("baf")), Map(NCName("z") -> Literal("w"),
                                                                                                                               NCName("a") -> Literal("bc"))),
                                          "list { token }"                 -> ApplyUnOp(UnOp("list"), PrimitiveDatatype("token")),
                                          "mixed { element foo { text } }" -> ApplyUnOp(UnOp("mixed"), Element(NCName("foo"), PrimitivePattern("text"))),
                                          "string , string"                -> ApplyBinOp(BinOp(","), PrimitiveDatatype("string"), PrimitiveDatatype("string")),
                                          "string & string"                -> ApplyBinOp(BinOp("&"), PrimitiveDatatype("string"), PrimitiveDatatype("string")),
                                          "string & (string | empty)"      -> ApplyBinOp(BinOp("&"), PrimitiveDatatype("string"), ApplyBinOp(BinOp("|"), 
                                                                                                                                             PrimitiveDatatype("string"),
                                                                                                                                             PrimitivePattern("empty"))),
                                          "string | empty"                 -> ApplyBinOp(BinOp("|"), PrimitiveDatatype("string"), PrimitivePattern("empty")),
                                          "element t:* { parent uM }"      -> Element(WildcardNameClass(Some(NCName("t"))),Parent(NCName("uM"))),
                                          "(element t:* { parent uM })+"   -> ApplyUnOp(UnOp("+"),Element(WildcardNameClass(Some(NCName("t"))),Parent(NCName("uM")))) )

    val declarations = Seq[(String, Declaration)](
      "namespace foo = inherit"                            -> Namespace(NCName("foo"), Right(Inherit())),
      "namespace foo = \"http://example.com/foo\""         -> Namespace(NCName("foo"), Left(new URI("http://example.com/foo"))),
      "default namespace = \"http://example.com/baz\""     -> DefaultNamespace(None, Left(new URI("http://example.com/baz"))),
      "default namespace biz = \"http://example.com/baz\"" -> DefaultNamespace(Some(NCName("biz")), Left(new URI("http://example.com/baz"))),
      "datatypes foo = \"http://example.com/dt\""          -> Datatypes(NCName("foo"), Literal("http://example.com/dt"))
    )

    val grammarContents = Seq[(String, GrammarContent)]("foo = baz" -> Define(NCName("foo"), AssignOp("="), NCNamePattern(NCName("baz"))),
                                                        "div { }"   -> Div(Seq()))

    val schemas = Seq[(String, Schema)](
      "empty"                            -> Schema(Seq(), Left(PrimitivePattern("empty"))),
      "start = empty"                    -> Schema(Seq(), Right(Seq(Define(NCName("start"), AssignOp("="), PrimitivePattern("empty"))))),
      "start = empty\n start |= string"  -> Schema(Seq(), Right(Seq(Define(NCName("start"), AssignOp("="), PrimitivePattern("empty")),
                                                                    Define(NCName("start"), AssignOp("|="), Datatype(PrimitiveDatatype("string")))))),
      "namespace foo = inherit\n start &= string" -> Schema(Seq(Namespace(NCName("foo"), Right(Inherit()))),
                                                            Right(Seq(Define(NCName("start"), AssignOp("&="), PrimitiveDatatype("string")))))
    )
  }

  /**
   * Examples with unusual whitespace or parens that should be eliminated by a pass through
   * the parser and pretty-printer. We still have that pretty(parse(<ast>)) == pretty(<ast>) for these
   * examples.
   */
  object NonCanonical {
    val nameClasses = Seq[(String, NameClass)]("(foo)"               -> NCName("foo"))
    
    val patterns = Seq("element foo {empty\n}"          -> Element(NCName("foo"), PrimitivePattern("empty")),
                       "(empty)"                        -> PrimitivePattern("empty"),
                       "attribute foo {empty}"          -> Attribute(NCName("foo"), PrimitivePattern("empty")),
                       "attribute foo {notAllowed}"     -> Attribute(NCName("foo"), PrimitivePattern("notAllowed")),
                       "string+"                        -> ApplyUnOp(UnOp("+"), PrimitiveDatatype("string")),
                       "string*"                        -> ApplyUnOp(UnOp("*"), PrimitiveDatatype("string")))
    
    val declarations = Seq()

    val grammarContent = Seq("foo = baz"                          -> Define(NCName("foo"), AssignOp("="), NCNamePattern(NCName("baz"))),
                             "div { }"                            -> Div(Seq()))

    val schemas = Seq("empty"                            -> Schema(Seq(), Left(PrimitivePattern("empty"))),
                      "start = empty"                    -> Schema(Seq(), Right(Seq(Define(NCName("start"), AssignOp("="), PrimitivePattern("empty"))))),
                      "start = empty\n start |= string"  -> Schema(Seq(), Right(Seq(Define(NCName("start"), AssignOp("="), PrimitivePattern("empty")),
                                                                                      Define(NCName("start"), AssignOp("|="), Datatype(PrimitiveDatatype("string")))))),
                      "namespace foo = inherit\n start &= string" -> Schema(Seq(Namespace(NCName("foo"), Right(Inherit()))),
                                                                            Right(Seq(Define(NCName("start"), AssignOp("&="), PrimitiveDatatype("string"))))))
  }
}
