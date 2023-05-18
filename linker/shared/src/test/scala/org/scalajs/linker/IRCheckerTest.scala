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

package org.scalajs.linker

import scala.concurrent._
import scala.util.{Failure, Success}

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.logging._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._
import org.scalajs.linker.testutils.CapturingLogger.LogLines

class IRCheckerTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  import IRCheckerTest._

  @Test
  def testMethodCallOnClassWithNoInstances(): AsyncResult = await {
    val FooClass = ClassName("Foo")
    val BarClass = ClassName("Bar")

    val methMethodName = m("meth", List(ClassRef(FooClass)), V)
    val nullBarMethodName = m("nullBar", Nil, ClassRef(BarClass))

    def callMethOn(receiver: Tree): Tree =
      Apply(EAF, receiver, methMethodName, List(Null()))(NoType)

    val classDefs = Seq(
        // LFoo will be dropped by base linking
        classDef("Foo", superClass = Some(ObjectClass)),

        classDef("Bar",
            superClass = Some(ObjectClass),
            methods = List(
                trivialCtor("Bar"),

                /* This method is called, but unreachable because there are no
                 * instances of `Bar`. It will therefore not make `Foo` reachable.
                 */
                MethodDef(EMF, methMethodName, NON,
                    List(paramDef("foo", ClassType("Foo"))), NoType,
                    Some(Skip()))(
                    EOH, UNV)
            )
        ),

        classDef(MainTestClassName,
            superClass = Some(ObjectClass),
            methods = List(
                trivialCtor(MainTestClassName),
                MethodDef(EMF.withNamespace(MemberNamespace.PublicStatic),
                    nullBarMethodName, NON, Nil, ClassType("Bar"),
                    Some(Null()))(
                    EOH, UNV),
                mainMethodDef(Block(
                    callMethOn(ApplyStatic(EAF, MainTestClassName,
                        nullBarMethodName, Nil)(ClassType("Bar"))),
                    callMethOn(Null()),
                    callMethOn(Throw(Null()))
                ))
            )
        )
    )

    testLinkNoIRError(classDefs, mainModuleInitializers("Test"))
  }

  @Test
  def missingJSNativeLoadSpec(): AsyncResult = await {
    val classDefs = Seq(
      classDef("A", kind = ClassKind.NativeJSClass, superClass = Some(ObjectClass)),
      classDef("B", kind = ClassKind.NativeJSClass, superClass = Some(ObjectClass)),
      classDef("C", kind = ClassKind.NativeJSModuleClass, superClass = Some(ObjectClass)),

      classDef("D", kind = ClassKind.JSClass, superClass = Some("A"), jsConstructor = Some(trivialJSCtor)),

      mainTestClassDef(Block(
        LoadJSConstructor("B"),
        LoadJSModule("C"),
        LoadJSConstructor("D")
      ))
    )

    for (log <- testLinkIRErrors(classDefs, MainTestModuleInitializers)) yield {
      log.assertContainsError(
          "Cannot load JS constructor of native JS class B without native load spec")
      log.assertContainsError(
          "Cannot load JS module of native JS module class C without native load spec")
      log.assertContainsError(
          "Native super class A must have a native load spec")
    }
  }

  @Test
  def jsClassConstructorBodyMustBeExpr1_Issue4491(): AsyncResult = await {
    val classDefs = Seq(
      JSObjectLikeClassDef,

      classDef(
        "Foo",
        kind = ClassKind.JSClass,
        superClass = Some(JSObjectLikeClass),
        jsConstructor = Some(
          JSConstructorDef(JSCtorFlags, Nil, None, JSConstructorBody(
            Nil,
            JSSuperConstructorCall(Nil),
            Nil
          ))(EOH, UNV)
        )
      ),

      mainTestClassDef(Block(
        LoadJSConstructor("Foo")
      ))
    )

    for (log <- testLinkIRErrors(classDefs, MainTestModuleInitializers)) yield {
      log.assertContainsError(
          "any expected but <notype> found for JS constructor body")
    }
  }

  @Test
  def jsClassConstructorBodyMustBeExpr2_Issue4491(): AsyncResult = await {
    val classDefs = Seq(
      JSObjectLikeClassDef,

      classDef(
        "Foo",
        kind = ClassKind.JSClass,
        superClass = Some(JSObjectLikeClass),
        jsConstructor = Some(
          JSConstructorDef(JSCtorFlags, Nil, None, JSConstructorBody(
            Nil,
            JSSuperConstructorCall(Nil),
            VarDef("x", NON, IntType, mutable = false, int(5)) :: Nil
          ))(EOH, UNV)
        )
      ),

      mainTestClassDef(Block(
        LoadJSConstructor("Foo")
      ))
    )

    for (log <- testLinkIRErrors(classDefs, MainTestModuleInitializers)) yield {
      log.assertContainsError(
          "any expected but <notype> found for JS constructor body")
    }
  }

}

object IRCheckerTest {
  def testLinkNoIRError(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer])(
      implicit ec: ExecutionContext): Future[Unit] = {
    link(classDefs, moduleInitializers, new ScalaConsoleLogger(Level.Error))
  }

  def testLinkIRErrors(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer])(
      implicit ec: ExecutionContext): Future[LogLines] = {

    val logger = new CapturingLogger

    link(classDefs, moduleInitializers, logger).transform {
      case Success(_) => Failure(new AssertionError("IR checking did not fail"))
      case Failure(_) => Success(logger.allLogLines)
    }
  }

  private def link(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer],
      logger: Logger)(implicit ec: ExecutionContext): Future[Unit] = {
    val config = StandardConfig()
      .withCheckIR(true)
      .withOptimizer(false)

    val noSymbolRequirements = SymbolRequirement
      .factory("IRCheckerTest")
      .none()

    val linkerFrontend = StandardLinkerFrontend(config, noSymbolRequirements)

    TestIRRepo.minilib.flatMap { stdLibFiles =>
      val irFiles = (
          stdLibFiles ++
          classDefs.map(MemClassDefIRFile(_))
      )

      linkerFrontend.link(irFiles, moduleInitializers, logger)
    }.map(_ => ())
  }
}
