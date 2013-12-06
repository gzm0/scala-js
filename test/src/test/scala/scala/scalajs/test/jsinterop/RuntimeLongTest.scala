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

/**
 * test the runtime Long implementation directly
 * does not depend on magic compiler Long rewriting
 */
object RuntimeLongTest extends ScalaJSTest {

  /** overload expect for long to add toString */
  def expect(l: Long): JasmineExpectation = expect(l.toString)

  describe("scala.scalajs.runtime.Long") {

    val maxInt = Long.fromInt(Int.MaxValue)
    val minInt = Long.fromInt(Int.MinValue)
    val one    = Long.fromInt(1)

    it("should correctly implement negation") {
      expect(-Long.fromInt(5)).toEqual("-5")
      expect(-Long.fromInt(0)).toEqual("0")
      expect(-minInt).toEqual("2147483648")
    }

    it("should correctly implement addition") {
      expect(Long.fromInt(7) + Long.fromInt(15)).toEqual("22")
      expect(maxInt + maxInt).toEqual("4294967294")
    }

    it("should correctly implement subtraction") {
      expect(Long.fromInt(7) - Long.fromInt(15)).toEqual("-8")
      expect(maxInt - maxInt).toEqual("0")
    }

    it("should correctly implement multiplication") {
      expect(Long.fromInt(7)  * Long.fromInt(15)).toEqual("105")
      expect(Long.fromInt(-7) * Long.fromInt(15)).toEqual("-105")
      expect(maxInt * maxInt).toEqual("4611686014132420609")
    }

    it("should correctly implement division") {
      expect(Long.fromInt(7)  / Long.fromInt(15)).toEqual("0")
      expect(Long.fromInt(24) / Long.fromInt(5)).toEqual("4")
      expect(Long.fromInt(24) / Long.fromInt(-5)).toEqual("-4")
      expect(maxInt / Long.fromInt(-5)).toEqual("429496729")
    }

    it("should correctly implement toString") {
      expect(maxInt).toEqual("2147483647")
      expect(maxInt+one).toEqual("2147483648")
      expect(minInt).toEqual("-2147483648")
    }

  }

}


