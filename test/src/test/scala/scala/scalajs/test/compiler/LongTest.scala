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
object LongTest extends ScalaJSTest {

  describe("JavaScript 64-bit long compatibility") {
    it("should correctly handle literals") {
      expect(5L + 100L).toEqual(105L)
      expect(2147483649L + 2L).toEqual(2147483651L)
      expect(-2147483648L * 4).toEqual(-8589934592L)
    }
  }
  
}