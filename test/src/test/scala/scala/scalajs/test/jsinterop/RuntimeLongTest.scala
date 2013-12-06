/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package jsinterop

import scala.scalajs.test.ScalaJSTest
import scala.scalajs.runtime.Long

object RuntimeLongTest extends ScalaJSTest {

  describe("scala.scalajs.runtime.Long") {

    it("should correctly implement negation") {
      expect((-Long.fromInt(5)).toString).toEqual("-5")
    }

  }

}


