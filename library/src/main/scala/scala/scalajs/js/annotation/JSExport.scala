/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.scalajs.js.annotation

/** Specifies that the given entity should be exported for use in raw JS */
class JSExport private (name: Option[String] = None, prop: Option[Boolean] = None) extends scala.annotation.StaticAnnotation {
  def this() = this(None)
  def this(name: String) = this(Some(name), None)
  def this(prop: Boolean) = this(None, Some(prop))
  def this(name: String, prop: Boolean) = this(Some(name), Some(prop))
}
