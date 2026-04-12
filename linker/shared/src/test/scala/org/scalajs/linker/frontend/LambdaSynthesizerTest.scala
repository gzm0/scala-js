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

package org.scalajs.linker.frontend

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.NewLambda.Descriptor
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.linker.testutils.TestIRBuilder._

class LambdaSynthesizerTest {
  private def makeDesc(superClass: ClassName, interfaces: Vector[ClassName],
      methodName: String, paramTypeRefs: Vector[TypeRef], resultTypeRef: TypeRef): Descriptor = {

    // Only for tests; would not work for JS class types
    def typeRefToType(typeRef: TypeRef): Type = typeRef match {
      case typeRef: PrimRef          => typeRef.tpe
      case ClassRef(className)       => ClassType(className, nullable = true, exact = false)
      case typeRef: ArrayTypeRef     => ArrayType(typeRef, nullable = true, exact = false)
      case typeRef: TransientTypeRef => typeRef.tpe
    }

    Descriptor(superClass, interfaces,
        MethodName(SimpleMethodName(methodName), paramTypeRefs, resultTypeRef),
        paramTypeRefs.map(typeRefToType(_)), typeRefToType(resultTypeRef))
  }

  private def makeClassName(superClass: ClassName, interfaces: Vector[ClassName],
      methodName: String, paramTypeRefs: Vector[TypeRef], resultTypeRef: TypeRef): String = {
    val desc = makeDesc(superClass, interfaces, methodName, paramTypeRefs, resultTypeRef)
    LambdaSynthesizer.makeClassName(desc).nameString
  }

  @Test def testMakeClassNameBasicShape(): Unit = {
    assertEquals(
        "java.lang.Comparable.$$Lambda$fa13d0f5607243329b6dbf6698569d230ec3ead0",
        makeClassName(ObjectClass, Vector("java.lang.Comparable"), "compareTo", Vector(O), I))

    assertEquals(
        "scala.runtime.AbstractFunction1.$$Lambda$7afc3dd0acc1681fb022ef921c83979087aaa919",
        makeClassName("scala.runtime.AbstractFunction1", Vector(), "apply", Vector(O), O))
  }

  @Test def testMakeClassNameEveryBitMatters(): Unit = {
    val IClass = ClassRef("I")
    val CClass = ClassRef("C")

    val descs: Vector[Descriptor] = Vector(
      makeDesc(ObjectClass, Vector("I"), "foo", Vector(IClass), I),
      makeDesc("A", Vector("I"), "foo", Vector(IClass), I),
      makeDesc(ObjectClass, Vector("I"), "foo", Vector(IClass, CharRef), I),
      makeDesc(ObjectClass, Vector("J"), "foo", Vector(IClass), I),
      makeDesc(ObjectClass, Vector("I"), "bar", Vector(IClass), I),
      makeDesc(ObjectClass, Vector("I"), "foo", Vector(CClass), I),
      makeDesc(ObjectClass, Vector("I"), "foo", Vector(IClass), Z),
      makeDesc(ObjectClass, Vector("I"), "foo", Vector(IClass), IClass),
      makeDesc(ObjectClass, Vector("I"), "foo", Vector(CClass), IClass),
      makeDesc(ObjectClass, Vector("I"), "foo", Vector(IClass), V),
      makeDesc(ObjectClass, Vector("I"), "foo", Vector(IClass), ArrayTypeRef(I, 1)),
      makeDesc(ObjectClass, Vector("I"), "foo", Vector(IClass), ArrayTypeRef(IClass, 1)),
      makeDesc(ObjectClass, Vector("I"), "foo", Vector(IClass), ArrayTypeRef(I, 3)),
      makeDesc(ObjectClass, Vector("I"), "foo", Vector(IClass), ArrayTypeRef(IClass, 3)),
      makeDesc(ObjectClass, Vector("I"), "foo", Vector(IClass), TransientTypeRef(LabelName("I"))(IntType))
    )

    val classNames = descs.map(LambdaSynthesizer.makeClassName(_))

    for {
      i <- 0 until descs.size
      j <- i + 1 until descs.size
    } {
      if (classNames(i) == classNames(j)) {
        fail(
            "Two descriptors hashed to the same class name:\n" +
            s"${descs(i)}\n${descs(j)}\n${classNames(i).nameString}")
      }
    }
  }
}
