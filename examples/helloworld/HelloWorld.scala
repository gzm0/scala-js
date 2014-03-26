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

  //@JSExport
  //def a(a: Int)(b: String = "asdf") = a
  
  @JSExport
  def a(a: Int, b: String) = a

  @JSExport
  def a(a: Int, b: Int, c: String) = a

  val x = this.asInstanceOf[js.Dynamic]

  println(x.a(1, ()))
  println(x.a(1,2))

}
