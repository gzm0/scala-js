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

package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

object SpecialNames {
  // Class names

  /* Our back-end-specific box classes for the generic representation of
   * `char` and `long`. These classes are not part of the classpath. They are
   * generated automatically by `DerivedClasses`.
   */
  val CharBoxClass = BoxedCharacterClass.withSuffix("Box")
  val LongBoxClass = BoxedLongClass.withSuffix("Box")

  val CharBoxCtor = MethodName.constructor(Vector(CharRef))
  val LongBoxCtor = MethodName.constructor(Vector(LongRef))

  val JLNumberClass = ClassName("java.lang.Number")

  // js.JavaScriptException, for WrapAsThrowable and UnwrapFromThrowable
  val JSExceptionClass = ClassName("scala.scalajs.js.JavaScriptException")

  val UndefinedBehaviorErrorClass =
    ClassName("org.scalajs.linker.runtime.UndefinedBehaviorError")

  val WasmRuntimeClass =
    ClassName("org.scalajs.linker.runtime.WasmRuntime")

  // Field names

  val valueFieldSimpleName = SimpleFieldName("value")

  val exceptionFieldName = FieldName(JSExceptionClass, SimpleFieldName("exception"))

  // Method names

  val AnyArgConstructorName = MethodName.constructor(Vector(ClassRef(ObjectClass)))
  val StringArgConstructorName = MethodName.constructor(Vector(ClassRef(BoxedStringClass)))
  val IntArgConstructorName = MethodName.constructor(Vector(IntRef))
  val ThrowableArgConsructorName = MethodName.constructor(Vector(ClassRef(ThrowableClass)))

  val hashCodeMethodName = MethodName("hashCode", Vector(), IntRef)

  val fmodfMethodName = MethodName("fmodf", Vector(FloatRef, FloatRef), FloatRef)
  val fmoddMethodName = MethodName("fmodd", Vector(DoubleRef, DoubleRef), DoubleRef)

  /** A unique simple method name to map all method *signatures* into `MethodName`s. */
  val normalizedSimpleMethodName = SimpleMethodName("m")
}
