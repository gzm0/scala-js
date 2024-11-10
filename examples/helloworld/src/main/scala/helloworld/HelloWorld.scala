/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import scala.annotation.{switch, tailrec}

import scala.scalajs.js
import js.annotation._

object HelloWorld {


  @noinline
  def makeReply(x: Long): Object = null

  @noinline
  def id(): Long = 1

  def main(args: Array[String]): Unit = {
    val callID: Long = id()

    val x = () => makeReply(callID)

    println(x)
  }


}
