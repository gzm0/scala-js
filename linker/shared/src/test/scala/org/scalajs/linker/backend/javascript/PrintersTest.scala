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

package org.scalajs.linker.backend.javascript

import scala.language.implicitConversions

import java.nio.charset.StandardCharsets

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir

import Trees._

class PrintersTest {
  import PrintersTest.FakeTransformed

  private implicit val pos: ir.Position = ir.Position.NoPosition

  private implicit def str2ident(name: String): Ident =
    Ident(name, ir.OriginalName.NoOriginalName)

  private def doPrint(tree: Tree): String = {
    val out = new ByteArrayWriter
    val printer = new Printers.JSTreePrinter(out)
    printer.printStat(tree)
    new String(out.toByteArray(), StandardCharsets.UTF_8)
  }

  private def assertPrintEquals(expected: String, tree: Tree): Unit = {
    val result = doPrint(tree)
    assertEquals(expected.stripMargin.trim + "\n", result)
  }

  @Test def printFunctionDef(): Unit = {
    assertPrintEquals(
        """
          |function test() {
          |  const x = 2;
          |  return x;
          |}
        """,
        FunctionDef("test", Nil, None, Block(
          Let("x", mutable = false, Some(IntLiteral(2))),
          Return(VarRef("x"))))
    )

    assertPrintEquals(
        """
          |function test() {
          |}
        """,
        FunctionDef("test", Nil, None, Skip())
    )
  }

  @Test def printClassDef(): Unit = {
    assertPrintEquals(
        """
          |class MyClass extends foo.Other {
          |}
        """,
        ClassDef(Some("MyClass"), Some(DotSelect(VarRef("foo"), "Other")), Nil)
    )

    assertPrintEquals(
        """
          |class MyClass {
          |  foo() {
          |  }
          |  get a() {
          |    return 1;
          |  }
          |  set a(x) {
          |  }
          |}
        """,
        ClassDef(Some("MyClass"), None, List(
          MethodDef(false, "foo", Nil, None, Skip()),
          GetterDef(false, "a", Return(IntLiteral(1))),
          SetterDef(false, "a", ParamDef("x"), Skip())
        ))
    )
  }

  @Test def printDocComment(): Unit = {
    assertPrintEquals(
      """
        | /** test */
      """,
      DocComment("test")
    )
  }

  @Test def printFor(): Unit = {
    assertPrintEquals(
      """
        |for (let x = 1; (x < 15); x = (x + 1)) {
        |}
      """,
      For(Let("x", true, Some(IntLiteral(1))),
          BinaryOp(ir.Trees.JSBinaryOp.<, VarRef("x"), IntLiteral(15)),
          Assign(VarRef("x"), BinaryOp(ir.Trees.JSBinaryOp.+, VarRef("x"), IntLiteral(1))),
          Skip())
    )
  }

  @Test def printForIn(): Unit = {
    assertPrintEquals(
      """
        |for (var x in foo) {
        |}
      """,
      ForIn(VarDef("x", None), VarRef("foo"), Skip())
    )
  }

  @Test def printIf(): Unit = {
    assertPrintEquals(
        """
          |if (false) {
          |  1;
          |}
        """,
        If(BooleanLiteral(false), IntLiteral(1), Skip())
    )

    assertPrintEquals(
        """
          |if (false) {
          |  1;
          |} else {
          |  2;
          |}
        """,
        If(BooleanLiteral(false), IntLiteral(1), IntLiteral(2))
    )

    assertPrintEquals(
        """
          |if (false) {
          |  1;
          |} else if (true) {
          |  2;
          |} else {
          |  3;
          |}
        """,
        If(BooleanLiteral(false), IntLiteral(1),
            If(BooleanLiteral(true), IntLiteral(2), IntLiteral(3)))
    )
  }

  @Test def disallowTransformed(): Unit = {
    assertThrows(classOf[IllegalArgumentException],
        () => doPrint(Transformed(FakeTransformed)))
  }

  @Test def showTransformed(): Unit = {
    val str = While(BooleanLiteral(false), Transformed(FakeTransformed)).show
    assertEquals(
      """
        |while (false) {
        |  transformed (org.scalajs.linker.backend.javascript.PrintersTest$FakeTransformed$) {
        |show
        |  }
        |}
      """.stripMargin.trim,
      str
    )
  }
}

object PrintersTest {
  object FakeTransformed extends Transformed.Value {
    def show: String = "show"
  }
}
