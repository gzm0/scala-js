/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.scalajs.js.annotation

/** Marks the targeted object (or potentially class) as JavaScript global scope
 *  
 *  Do not use in code. Is automatically added by the compiler.
 *  Inherit from scala.scalajs.js.GlobalScope instead.
 */
class JSGlobalScope extends scala.annotation.StaticAnnotation
