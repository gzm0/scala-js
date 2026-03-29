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

package org.scalajs.linker.backend.emitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

private[emitter] object EmitterNames {
  // Class names

  val JavaScriptExceptionClass =
    ClassName("scala.scalajs.js.JavaScriptException")

  val UndefinedBehaviorErrorClass =
    ClassName("org.scalajs.linker.runtime.UndefinedBehaviorError")

  val FloatingPointBitsPolyfillsClass =
    ClassName("org.scalajs.linker.runtime.FloatingPointBitsPolyfills")

  // Field names

  val exceptionFieldName = FieldName(JavaScriptExceptionClass, SimpleFieldName("exception"))

  // Method names

  val AnyArgConstructorName = MethodName.constructor(Vector(ClassRef(ObjectClass)))
  val IntArgConstructorName = MethodName.constructor(Vector(IntRef))
  val StringArgConstructorName = MethodName.constructor(Vector(ClassRef(BoxedStringClass)))
  val ThrowableArgConsructorName = MethodName.constructor(Vector(ClassRef(ThrowableClass)))

  val cloneMethodName = MethodName("clone", Vector.empty, ClassRef(ObjectClass))
  val getClassMethodName = MethodName("getClass", Vector.empty, ClassRef(ClassClass))
  val hashCodeMethodName = MethodName("hashCode", Vector.empty, IntRef)
  val toStringMethodName = MethodName("toString", Vector.empty, ClassRef(BoxedStringClass))

  val getNameMethodName = MethodName("getName", Vector.empty, ClassRef(BoxedStringClass))
  val getSuperclassMethodName = MethodName("getSuperclass", Vector.empty, ClassRef(ClassClass))

  val floatToBits = MethodName("floatToBits", Vector(FloatRef), IntRef)
  val floatFromBits = MethodName("floatFromBits", Vector(IntRef), DoubleRef) // yes, Double
  val doubleToBits = MethodName("doubleToBits", Vector(DoubleRef), LongRef)
  val doubleFromBits = MethodName("doubleFromBits", Vector(LongRef), DoubleRef)
}
