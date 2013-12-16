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
      expect(4503599627370510L * (-4)).toEqual(-18014398509482040L)
    }
    
    it("should correctly dispatch unary ops on Longs") {
      val x = 10L
      expect(-x == -10L).toBeTruthy
      val y = 5L
      expect(-y).toEqual(-5L)
      expect(+y).toEqual(5L)
      expect(~y).toEqual(-6L)
    }
    
    it("should correctly dispatch binary ops on unboxed Longs") {
      expect(5L * 5F).toEqual(25F)
      expect(5F * 4L).toEqual(20F)
    }
    
    it("primitives should convert to Long") {
      // Byte
      expect(234.toByte.toLong).toEqual(234L)
      // Short
      expect((-10).toShort.toLong).toEqual(-10L)
      // Char
      expect('A'.toLong).toEqual(65L)
      // Int
      expect(5.toLong).toEqual(5L)
      // Long
      expect(10L.toLong).toEqual(10L)
      // Float
      expect(100000.6f.toLong).toEqual(100000L)
      // Double
      expect(100000.6.toLong).toEqual(100000L)
    }
    
    it("should generate a hash") {
      val x = 5L
      val y = 5L
      expect(x.##).toEqual(y.##)
    } 
    
    
    it("should correctly concat to string") {
      val x = 20L
      expect("asdf" + 5L + x + "hello").toEqual("asdf520hello")
      expect(x + "hello").toEqual("20hello")
    }
    
    it("string should convert to Long") {
      expect("45678901234567890".toLong).toEqual(45678901234567890L)
    }
  }
  
}