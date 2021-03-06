RelaxNG Compact Syntax Parser and Pretty-Printer for Scala
==========================================================

This library provides classes for working with Compact-syntax RelaxNG grammars
in a Scala-esque / functional way. It is NOT an XML Validator.

The components are:

 1. A comprehensive abstract syntax for RelaxNG Compact Syntax, with Arbitrary instances
 2. A parser for the abstract syntax
 3. A pretty-printer for the abstract syntax
 4. A basic simplifier, primarily for resolving local include directives

For now, use via "sbt publish-local" and add this to your build.sbt 

`libraryDependencies += "com.inkling" %% "relaxng" % "0.1"`

Dependencies:
-------------

Dependencies are fetched automatically via SBT.

 * [SBT](https://github.com/harrah/xsbt): The library is built using SBT >= 0.10.0
   - To buld, run "sbt compile" in the root directory.
   - To test, run "sbt test" in the root directory
 * [ScalaCheck](http://code.google.com/p/scalacheck/): The library provides scalacheck Arbitrary instances, hence requires it even for non-testing use.
 * [ScalaTest](http://www.scalatest.org/): For running the test suites, ScalaTest is required

Contributors
------------

 * [Kenn Knowles](https://github.com/kennknowles) ([@kennknowles](https://twitter.com/KennKnowles))
 * [Arthur Kopatsy](https://github.com/kopatsy) ([@akopatsy](https://twitter.com/akopatsy))

Licenses of Dependencies
------------------------   
 * ScalaTest is distributed under the Apache 2.0 License 
 * ScalaCheck is distributed under a modified BSD License: http://scalacheck.googlecode.com/svn/artifacts/1.9/doc/LICENSE

Copyright and License
---------------------

Copyright 2011-2012 Inkling Systems, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
