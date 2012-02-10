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

import Parsers._
import Pretty._

import java.io.File
import java.net.URI

/**
 * Main object for pretty-printing RelaxNg schema
 */

object Main {
  def main(args: Array[String]) {
    for(filename <- args) {
      val f = new File(filename)
      load(f) match {
        case Success(schema, _) =>
          //println(schema)
          println(prettyString(Flatten.flatten(schema, relativeTo = new URI("file://%s".format(f.getAbsolutePath)))))

        case err  => println("Parse failure for %s: %s".format(filename, err))
      }
    }
  }
}
