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

    val identifiers = Seq[(String, NCName)]("foo"          -> NCName("foo"),
                                            "x.y"          -> NCName("x.y"),
                                            "text.special" -> NCName("text.special"))
    
    val params = Seq[(String, Map[NCName, Literal])]("{ foo = \"bar\" }" -> Map(NCName("foo") -> Literal("bar")),
                                                     "{ foo = \"bar\" foo2 = \"bar2\"}" -> Map(NCName("foo") -> Literal("bar"),
                                                                                               NCName("foo2") -> Literal("bar2")),
                                                     "{ foo= \"bar\" }" -> Map(NCName("foo") -> Literal("bar")))
                                          
    val applyUnOps = Seq[(String, ApplyUnOp)]("element t:* { parent uM } ?"     -> ApplyUnOp(UnOp("?"), Element(WildcardNameClass(Some(NCName("t"))),Parent(NCName("uM")))))

    val patterns = Seq[(String, Pattern)](
      "empty"                          -> NCNamePattern(NCName("empty")),
      "bizzle"                         -> NCNamePattern(NCName("bizzle")),
      "foo.baz"                        -> NCNamePattern(NCName("foo.baz")),
      "text.ident"                     -> NCNamePattern(NCName("text.ident")),
      "element foo { empty }"          -> Element(NCName("foo"), NCNamePattern(NCName("empty"))),
      "attribute foo { empty }"        -> Attribute(NCName("foo"), NCNamePattern(NCName("empty"))),
      "attribute foo { notAllowed }"   -> Attribute(NCName("foo"), NCNamePattern(NCName("notAllowed"))),
      "string +"                       -> ApplyUnOp(UnOp("+"), PrimitiveDatatype("string")),
      "string *"                       -> ApplyUnOp(UnOp("*"), PrimitiveDatatype("string")),
      "xsd:bif { x = \"y\"}"           -> Datatype(CName(NCName("xsd"), NCName("bif")), Map(NCName("x") -> Literal("y"))),
      "s9:baf { z = \"w\" a = \"bc\"}" -> Datatype(CName(NCName("s9"), NCName("baf")), Map(NCName("z") -> Literal("w"),
                                                                                           NCName("a") -> Literal("bc"))),
      "list { token }"                 -> ApplyUnOp(UnOp("list"), PrimitiveDatatype("token")),
      "mixed { element foo { text } }" -> ApplyUnOp(UnOp("mixed"), Element(NCName("foo"), NCNamePattern(NCName("text")))),
      "string , string"                -> ApplyBinOp(BinOp(","), PrimitiveDatatype("string"), PrimitiveDatatype("string")),
      "string & string"                -> ApplyBinOp(BinOp("&"), PrimitiveDatatype("string"), PrimitiveDatatype("string")),
      "string & (string | empty)"      -> ApplyBinOp(BinOp("&"), PrimitiveDatatype("string"), ApplyBinOp(BinOp("|"), 
                                                                                                         PrimitiveDatatype("string"),
                                                                                                         NCNamePattern(NCName("empty")))),
      "string | empty"                 -> ApplyBinOp(BinOp("|"), PrimitiveDatatype("string"), NCNamePattern(NCName("empty"))),
      "element t:* { parent uM }"      -> Element(WildcardNameClass(Some(NCName("t"))),Parent(NCName("uM"))),
      "element t:* { parent uM }?"     -> ApplyUnOp(UnOp("?"), Element(WildcardNameClass(Some(NCName("t"))),Parent(NCName("uM")))),
      "element t:* { parent uM }+"   -> ApplyUnOp(UnOp("+"),Element(WildcardNameClass(Some(NCName("t"))),Parent(NCName("uM")))),

      "element foo { empty } | element baz { empty }" -> ApplyBinOp(BinOp("|"),
                                                                    Element(NCName("foo"), NCNamePattern(NCName("empty"))),
                                                                    Element(NCName("baz"), NCNamePattern(NCName("empty")))),
      "element foo { empty } , element baz { empty }" -> ApplyBinOp(BinOp(","),
                                                                    Element(NCName("foo"), NCNamePattern(NCName("empty"))),
                                                                    Element(NCName("baz"), NCNamePattern(NCName("empty")))),
      "element foo { empty } , attribute baz { x.y }" -> ApplyBinOp(BinOp(","),
                                                                    Element(NCName("foo"), NCNamePattern(NCName("empty"))),
                                                                    Attribute(NCName("baz"), NCNamePattern(NCName("x.y"))))
    )

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
      "empty"                            -> Schema(Seq(), Left(NCNamePattern(NCName("empty")))),
      "element foo { empty }"            -> Schema(Seq(), Left(Element(NCName("foo"), NCNamePattern(NCName("empty"))))),
      "start = empty"                    -> Schema(Seq(), Right(Seq(Define(NCName("start"), AssignOp("="), NCNamePattern(NCName("empty")))))),
      "foo |= empty"                    -> Schema(Seq(), Right(Seq(Define(NCName("foo"), AssignOp("|="), NCNamePattern(NCName("empty")))))),
      "start = element foo { empty }"    -> Schema(Seq(), Right(Seq(Define(NCName("start"), AssignOp("="), Element(NCName("foo"), NCNamePattern(NCName("empty"))))))),
      "fizzle = element foo { empty }"    -> Schema(Seq(), Right(Seq(Define(NCName("fizzle"), AssignOp("="), Element(NCName("foo"), NCNamePattern(NCName("empty"))))))),
      "start = empty\n start |= string"  -> Schema(Seq(), Right(Seq(Define(NCName("start"), AssignOp("="), NCNamePattern(NCName("empty"))),
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
    
    val patterns = Seq("element foo {empty\n}"          -> Element(NCName("foo"), NCNamePattern(NCName("empty"))),
                       "(empty)"                        -> NCNamePattern(NCName("empty")),
                       "attribute foo {empty}"          -> Attribute(NCName("foo"), NCNamePattern(NCName("empty"))),
                       "attribute foo {notAllowed}"     -> Attribute(NCName("foo"), NCNamePattern(NCName("notAllowed"))),
                       "string+"                        -> ApplyUnOp(UnOp("+"), PrimitiveDatatype("string")),
                       "string*"                        -> ApplyUnOp(UnOp("*"), PrimitiveDatatype("string")))
    
    val declarations = Seq()

    val grammarContent = Seq("foo = baz"                          -> Define(NCName("foo"), AssignOp("="), NCNamePattern(NCName("baz"))),
                             "div { }"                            -> Div(Seq()))

    val schemas = Seq("start = empty\n\n\nstart |= string"  -> Schema(Seq(), Right(Seq(Define(NCName("start"), AssignOp("="), NCNamePattern(NCName("empty"))),
                                                                                       Define(NCName("start"), AssignOp("|="), Datatype(PrimitiveDatatype("string")))))),
                      "namespace foo = inherit \n \n start &= string" -> Schema(Seq(Namespace(NCName("foo"), Right(Inherit()))),
                                                                                Right(Seq(Define(NCName("start"), AssignOp("&="), PrimitiveDatatype("string"))))))
  }

  object Rejects {
    val identifiers = Seq[String]("text")
  }
}
