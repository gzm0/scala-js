/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.nscplugin.test

import org.scalajs.nscplugin.test.util._

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.junit.Assert._
import org.junit.Test

class StaticForwardersASTTest extends JSASTTest {

  @Test
  def emitStaticForwardersInExistingClass(): Unit = {
    val classDef = """
    import scala.scalajs.js, js.annotation._

    class Foo(val y: Int = 10)

    object Foo {
      def bar(x: Int = 5): Int = x + 1

      @js.native
      @JSGlobal("foobar")
      def foobar(x: Int = 5): Int = js.native
    }
    """.extractOne("class Foo") {
      case cd: ClassDef if cd.name.name == ClassName("Foo") => cd
    }

    val staticMethodNames = classDef.methods
      .withFilter(_.flags.namespace.isStatic)
      .map(_.name.name)
      .sortBy(_.simpleName)

    assertEquals(
      Vector(
        MethodName("$lessinit$greater$default$1", Vector(), IntRef),
        MethodName("bar", Vector(IntRef), IntRef),
        MethodName("bar$default$1", Vector(), IntRef)
      ),
      staticMethodNames
    )
  }

  @Test
  def emitStaticForwardersInSyntheticClass(): Unit = {
    val classDef = """
    import scala.scalajs.js, js.annotation._

    object Foo {
      def bar(x: Int = 5): Int = x + 1

      @js.native
      @JSGlobal("foobar")
      def foobar(x: Int = 5): Int = js.native
    }
    """.extractOne("class Foo") {
      case cd: ClassDef if cd.name.name == ClassName("Foo") => cd
    }

    val staticMethodNames = classDef.methods
      .withFilter(_.flags.namespace.isStatic)
      .map(_.name.name)
      .sortBy(_.simpleName)

    assertEquals(
      Vector(
        MethodName("bar", Vector(IntRef), IntRef),
        MethodName("bar$default$1", Vector(), IntRef)
      ),
      staticMethodNames
    )
  }

}
