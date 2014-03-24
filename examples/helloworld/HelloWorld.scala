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
  def a(a: Int, b: Int = 5) = a + b

  val x = this.asInstanceOf[js.Dynamic]

  println(x.a(1))
  println(x.a(1,2))

}
