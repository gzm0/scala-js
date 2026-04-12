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

package org.scalajs.ir

import org.junit.Test
import org.junit.Assert._

import Names._
import Types._
import WellKnownNames._

class NamesTest {
  @Test def nameStringLocalName(): Unit = {
    assertEquals("foo", LocalName("foo").nameString)
    assertEquals(".this", LocalName.This.nameString)
  }

  @Test def nameStringLabelName(): Unit =
    assertEquals("foo", LabelName("foo").nameString)

  @Test def nameStringSimpleFieldName(): Unit =
    assertEquals("foo", SimpleFieldName("foo").nameString)

  @Test def nameStringFieldName(): Unit = {
    assertEquals("a.B+:foo",
        FieldName(ClassName("a.B"), SimpleFieldName("foo")).nameString)
  }

  @Test def nameStringSimpleMethodName(): Unit = {
    assertEquals("foo", SimpleMethodName("foo").nameString)
    assertEquals("<init>", SimpleMethodName.Constructor.nameString)
    assertEquals("<stinit>", SimpleMethodName.StaticInitializer.nameString)
    assertEquals("<clinit>", SimpleMethodName.ClassInitializer.nameString)
  }

  @Test def nameStringMethodName(): Unit = {
    assertEquals("foo;I", MethodName("foo", Vector(), IntRef).nameString)
    assertEquals("foo;Z;I", MethodName("foo", Vector(BooleanRef), IntRef).nameString)
    assertEquals("foo;Z;V", MethodName("foo", Vector(BooleanRef), VoidRef).nameString)

    assertEquals("foo;S;Ljava.io.Serializable;V",
        MethodName("foo", Vector(ShortRef, ClassRef(SerializableClass)), VoidRef).nameString)

    assertEquals("<init>;I;V", MethodName.constructor(Vector(IntRef)).nameString)

    assertEquals("foo;Z;R", MethodName.reflectiveProxy("foo", Vector(BooleanRef)).nameString)

    val refAndNameStrings: Vector[(TypeRef, String)] = Vector(
      ClassRef(ObjectClass) -> "Ljava.lang.Object",
      ClassRef(SerializableClass) -> "Ljava.io.Serializable",
      ClassRef(BoxedStringClass) -> "Ljava.lang.String",
      ArrayTypeRef(ClassRef(ObjectClass), 2) -> "[[Ljava.lang.Object",
      ArrayTypeRef(ShortRef, 1) -> "[S",
      TransientTypeRef(LabelName("bar"))(CharType) -> "tbar"
    )
    for ((ref, nameString) <- refAndNameStrings) {
      assertEquals(s"foo;$nameString;V",
          MethodName("foo", Vector(ref), VoidRef).nameString)
    }
  }

  @Test def nameStringClassName(): Unit =
    assertEquals("a.B", ClassName("a.B").nameString)
}
