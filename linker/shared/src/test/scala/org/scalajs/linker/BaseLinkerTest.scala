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

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.junit.async._

import org.scalajs.logging._

import org.scalajs.linker.checker._
import org.scalajs.linker.interface.StandardConfig
import org.scalajs.linker.standard._

import org.scalajs.linker.testutils.TestIRBuilder._
import org.scalajs.linker.testutils.LinkingUtils._

class BaseLinkerTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  @Test
  def noUnnecessaryDefaultBridges(): AsyncResult = await {
    val fooName = m("foo", Vector(), IntRef)
    val classDefs = Seq(
      classDef(
        "Intf",
        kind = ClassKind.Interface,
        methods = Vector(
            MethodDef(EMF, fooName, NON, Vector(), IntType, Some(int(1)))(EOH, UNV))
      ),
      classDef(
        "Base",
        kind = ClassKind.Class,
        superClass = Some(ObjectClass),
        interfaces = Vector("Intf"),
        methods = Vector(trivialCtor("Base"))
      ),
      classDef(
        "Sub",
        kind = ClassKind.Class,
        superClass = Some("Base"),
        methods = Vector(trivialCtor("Sub", "Base"))
      ),
      mainTestClassDef(
        consoleLog(Apply(EAF, New("Sub", NoArgConstructorName, Vector()), fooName, Vector())(IntType))
      )
    )

    val config = StandardConfig().withOptimizer(false)

    for (moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers, config = config)) yield {
      val clazz = findClass(moduleSet, "Sub").get
      assertFalse(clazz.methods.exists(_.name.name == fooName))
    }
  }

  @Test
  def correctThisTypeInHijackedClassReflectiveProxies_Issue4982(): AsyncResult = await {
    val compareTo = m("compareTo", Vector(ClassRef(BoxedIntegerClass)), IntRef)
    val compareToReflProxy =
      MethodName.reflectiveProxy("compareTo", Vector(ClassRef(BoxedIntegerClass)))

    val classDefs = Seq(
      mainTestClassDef(
        consoleLog(Apply(EAF, IntLiteral(5), compareToReflProxy, Vector(IntLiteral(6)))(AnyType))
      )
    )

    val config = StandardConfig().withOptimizer(false)

    for (moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers, config = config)) yield {
      val clazz = findClass(moduleSet, BoxedIntegerClass).get
      val previousPhase = CheckingPhase.BaseLinker
      val errorCount = ClassDefChecker.check(clazz, previousPhase,
          new ScalaConsoleLogger(Level.Error))
      assertEquals(0, errorCount)
    }
  }

  private def findClass(moduleSet: ModuleSet, name: ClassName): Option[LinkedClass] =
    moduleSet.modules.flatMap(_.classDefs).find(_.className == name)
}
