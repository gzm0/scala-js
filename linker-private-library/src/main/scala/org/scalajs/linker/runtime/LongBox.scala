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

package org.scalajs.linker.runtime

import java.lang.constant.{Constable, ConstantDesc}
import java.lang.{Number, Comparable}

class LongBox(val value: scala.Long) extends Number with Comparable[java.lang.Long] with Constable with ConstantDesc {
  @inline def longValue(): scala.Long = value.longValue()
  @inline override def byteValue(): scala.Byte = value.byteValue()
  @inline override def shortValue(): scala.Short = value.shortValue()
  @inline def intValue(): scala.Int = value.intValue()
  @inline def floatValue(): scala.Float = value.floatValue()
  @inline def doubleValue(): scala.Double = value.doubleValue()
  @inline override def equals(that: Any): scala.Boolean = value.equals(that)
  @inline override def hashCode(): Int = value.hashCode()
  @inline override def compareTo(that: java.lang.Long): Int = value.compareTo(that)
  @inline override def toString(): String = value.toString()

  // Unreachable (not in our javalib)
  def describeConstable(): java.util.Optional[_ <: ConstantDesc] = null
  def resolveConstantDesc(lookup: java.lang.invoke.MethodHandles.Lookup): Object = null
}
