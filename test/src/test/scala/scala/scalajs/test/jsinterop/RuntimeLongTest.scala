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
  def expect(l: Long): JasmineExpectation = expect(l.toHexString)

  describe("scala.scalajs.runtime.Long") {

    val maxInt = Long.fromInt(Int.MaxValue)
    val minInt = Long.fromInt(Int.MinValue)
    val one    = Long.fromInt(1)

    it("should correctly implement negation") {
      expect(-Long.fromInt(5)).toEqual("fffffffffffffffb")
      expect(-Long.fromInt(0)).toEqual("0000000000000000")
      expect(-minInt).toEqual(         "0000000080000000")
    }

    it("should correctly implement addition") {
      expect(Long.fromInt(7) + Long.fromInt(15)).toEqual("0000000000000016")
      expect(maxInt + maxInt).toEqual(                   "00000000fffffffe")
    }

    it("should correctly implement subtraction") {
      expect(Long.fromInt(7) - Long.fromInt(15)).toEqual("fffffffffffffff8")
      expect(maxInt - maxInt).toEqual(                   "0000000000000000")
    }

    it("should correctly implement multiplication") {
      expect(Long.fromInt(7)  * Long.fromInt(15)).toEqual("0000000000000069")
      expect(Long.fromInt(-7) * Long.fromInt(15)).toEqual("ffffffffffffff97")
      expect(maxInt * maxInt).toEqual(                    "3fffffff00000001")
    }

    it("should correctly implement division") {
      expect(Long.fromInt(7)  / Long.fromInt(15)).toEqual("0000000000000000")
      expect(Long.fromInt(24) / Long.fromInt(5)).toEqual( "0000000000000004")
      expect(Long.fromInt(24) / Long.fromInt(-5)).toEqual("fffffffffffffffc")
      expect(maxInt / Long.fromInt(-5)).toEqual(          "ffffffffe6666667")
    }

    it("should correctly implement toString") {
      expect(maxInt.toString).toEqual("2147483647")
      expect((maxInt+one).toString).toEqual("2147483648")
      expect(minInt.toString).toEqual("-2147483648")
    }

  }

}


