/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import scala.scalajs.js
import js.annotation._

object HelloWorld {
  @js.native
  @JSImport("foo.js")
  def bar(): Int = js.native

  def main(args: Array[String]): Unit = {
    println(bar())
  }
}
