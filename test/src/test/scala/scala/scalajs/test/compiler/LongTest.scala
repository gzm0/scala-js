/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package compiler

import scala.scalajs.test.ScalaJSTest

/**
 * tests the compiler re-patching of native longs to
 * scala.scalajs.runtime.Long
 * see scala.scalajs.test.jsinterop.RuntimeLongTest
 * for a test of the implementation itself
 */
class LongTest extends ScalaJSTest {

  describe("JavaScript 64-bit long compatibility") {
    it("handle appropriately large numbers") {
      expect(5L + 100L).toEqual(105L)
    }
  }
  
}