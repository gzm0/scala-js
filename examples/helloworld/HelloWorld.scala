/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import scala.scalajs.js
import js.annotation.{ JSName, JSExport }

trait Foo {
  @JSExport
  def x: Int
}

trait Bar extends Foo {
  @JSExport(name = "hello")
  @JSExport(name = "myawesomemethod")
  def x: Int
  def y(foo: String): String = "Bar" + foo
  def z = y("hello")
  def r = y _
}

class FooBarImpl extends Bar {
  def x: Int = 1
  @JSExport
  override def y(foo: String): String = "asdf" + foo
}

class A
class B extends A

class C1 {
  @JSExport
  def x: A = new A
}

class C2 extends C1 {
  @JSExport
  override def x: A = new B
}

//@JSExport(name = "fooo")
object HelloWorld {
  def hello = "hello world"
}