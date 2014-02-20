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
  @JSExport
  def z(x: Int): Int
}

trait Bar extends Foo {
  // Multiple exports
  @JSExport(name = "hello")
  @JSExport(name = "myawesomemethod")
  def x: Int
  def y(foo: String): String = "Bar" + foo
  
  @JSExport
  def w: Int
}

trait Stack extends Foo {
  // Export on absoverride
  @JSExport
  abstract override def x: Int = super.x + 1
}

class FooBarImpl extends Bar {
  def x: Int = 1
  
  // Export of overridden method without export in super
  @JSExport
  override def y(foo: String): String = "asdf" + foo
  
  // Forced inherited export
  def z(x: Int) = x + 5
  
  // Export twice (in subclass and superclass)
  @JSExport
  def w: Int = 5
  
  @JSExport
  val a: Int = 4
  
  @JSExport
  def mem = 1
}

class SubFooBarImpl extends FooBarImpl {
  
  // Export again (shouldn't generate method)
  @JSExport
  override def mem: Int = 5
  
}

// Conflicting exports. Should fail!
class Confl {
  
  class Box[T](val x: T)
  
  @JSExport(name = "value1")
  def hello = "foo"
    
  @JSExport(name = "value2")
  def world = "bar"
    
  @JSExport(name = "erasure1")
  def ub(x: Box[String]): String = x.x
  @JSExport(name = "erasure")
  def ub(x: Box[Int]): Int = x.x
  
  @JSExport(name = "rtType1")
  def rtType(x: Short) = x
  
  @JSExport(name = "rtType")
  def rtType(x: Int) = x
}

// Exports in object
object A {
  @JSExport
  def value = 1

  // Export a generic
  @JSExport
  def gen[T](x: T) = x
  
  // Export lambda ret value
  @JSExport
  def lambda(x: Int) = (y: Int) => x + y
  
  // Export multi param list
  @JSExport
  def multiParam(x: Int)(y: Int): Int = 1
  
  // Export default arguments
  @JSExport
  def defArg(x: Int = 1) = x
  
  // Something no one should export
  @JSExport
  def ahem[T : Comparable](x: T)(implicit y: Int) = ???
}

class A
class B extends A

class C1 {
  @JSExport
  def x: A = new A
}

class C2 extends C1 {
  // Bridged export
  @JSExport
  override def x: B = new B
}

//@JSExport(name = "fooo")
object HelloWorld {
  
  def hello = "hello world"
}