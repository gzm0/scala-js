/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import scala.scalajs.js
import js.annotation.{ JSName, JSExport }

@JSExport
object HelloWorld extends App {

  @JSExport
  def a(a: Int)(b: Int = 5)(c: Int = 7) = a + b + c

  @JSExport
  def b(a: Int)(b: String = "asdf") = s"$a $b"
  
  @JSExport
  def b(a: Int, b: js.Undefined) = "woot"
  
  @JSExport
  def a(a: Int, b: String) = a

  @JSExport
  def a(a: Int, b: Int, c: String) = a

  @JSExport
  def foo(x: Int = 1) = x
  @JSExport
  def foo(x: String*) = x
  
  val x = this.asInstanceOf[js.Dynamic]

  println(x.a(1, ()))
  println(x.a(1,2))
  println(x.b(1))
  println(x.b(1, ()))

}
