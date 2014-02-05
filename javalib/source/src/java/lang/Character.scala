package java.lang

import scala.scalajs.js

class Character(value: scala.Char) {
  def charValue(): scala.Char = value

  override def equals(that: Any) =
    that.isInstanceOf[Character] && (value == that.asInstanceOf[Character].charValue)

  override def toString: String =
    js.Dynamic.global.String.fromCharCode(value.toInt).asInstanceOf[js.String]

  /*
   * Methods on scala.Char
   * The following methods are only here to properly support reflective calls
   * on boxed primitive values. YOU WILL NOT BE ABLE TO USE THESE METHODS, since
   * we use the true javalib to lookup symbols, this file contains only
   * implementations.
   */
  def toByte: scala.Byte     = value.toByte
  def toShort: scala.Short   = value.toShort
  def toChar: scala.Char     = value.toChar
  def toInt: scala.Int       = value
  def toLong: scala.Long     = value.toLong
  def toFloat: scala.Float   = value.toFloat
  def toDouble: scala.Double = value.toDouble

  /**
 * Returns the bitwise negation of this value.
 * @example {{{
 * ~5 == -6
 * // in binary: ~00000101 ==
 * //             11111010
 * }}}
 */
  def unary_~ : scala.Int = ~value
  /**
 * Returns this value, unmodified.
 */
  def unary_+ : scala.Int = value
  /**
 * Returns the negation of this value.
 */
  def unary_- : scala.Int = -value

  def +(x: String): String = value + x

  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  def <<(x: scala.Int): scala.Int = value << x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  def <<(x: scala.Long): scala.Int = value << x
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  *         filling the new left bits with zeroes.
  * @example {{{ 21 >>> 3 == 2 // in binary: 010101 >>> 3 == 010 }}}
  * @example {{{
  * -21 >>> 3 == 536870909
  * // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==
  * //            00011111 11111111 11111111 11111101
  * }}}
  */
  def >>>(x: scala.Int): scala.Int = value >>> x
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  *         filling the new left bits with zeroes.
  * @example {{{ 21 >>> 3 == 2 // in binary: 010101 >>> 3 == 010 }}}
  * @example {{{
  * -21 >>> 3 == 536870909
  * // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==
  * //            00011111 11111111 11111111 11111101
  * }}}
  */
  def >>>(x: scala.Long): scala.Int = value >>> x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the right bits with the same value as the left-most bit of this.
  *         The effect of this is to retain the sign of the value.
  * @example {{{
  * -21 >> 3 == -3
  * // in binary: 11111111 11111111 11111111 11101011 >> 3 ==
  * //            11111111 11111111 11111111 11111101
  * }}}
  */
  def >>(x: scala.Int): scala.Int = value >> x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the right bits with the same value as the left-most bit of this.
  *         The effect of this is to retain the sign of the value.
  * @example {{{
  * -21 >> 3 == -3
  * // in binary: 11111111 11111111 11111111 11101011 >> 3 ==
  * //            11111111 11111111 11111111 11111101
  * }}}
  */
  def >>(x: scala.Long): scala.Int = value >> x

  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Byte): scala.Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Short): scala.Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Char): scala.Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Int): scala.Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Long): scala.Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Float): scala.Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Double): scala.Boolean = value < x

  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Byte): scala.Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Short): scala.Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Char): scala.Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Int): scala.Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Long): scala.Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Float): scala.Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Double): scala.Boolean = value <= x

  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Byte): scala.Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Short): scala.Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Char): scala.Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Int): scala.Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Long): scala.Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Float): scala.Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Double): scala.Boolean = value > x

  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Byte): scala.Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Short): scala.Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Char): scala.Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Int): scala.Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Long): scala.Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Float): scala.Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Double): scala.Boolean = value >= x

  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: scala.Byte): scala.Int = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: scala.Short): scala.Int = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: scala.Char): scala.Int = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: scala.Int): scala.Int = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: scala.Long): scala.Long = value | x

  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: scala.Byte): scala.Int = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: scala.Short): scala.Int = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: scala.Char): scala.Int = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: scala.Int): scala.Int = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: scala.Long): scala.Long = value & x

  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: scala.Byte): scala.Int = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: scala.Short): scala.Int = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: scala.Char): scala.Int = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: scala.Int): scala.Int = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: scala.Long): scala.Long = value ^ x

  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Byte): scala.Int = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Short): scala.Int = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Char): scala.Int = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Int): scala.Int = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Long): scala.Long = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Float): scala.Float = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Double): scala.Double = value + x

  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Byte): scala.Int = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Short): scala.Int = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Char): scala.Int = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Int): scala.Int = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Long): scala.Long = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Float): scala.Float = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Double): scala.Double = value - x

  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Byte): scala.Int = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Short): scala.Int = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Char): scala.Int = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Int): scala.Int = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Long): scala.Long = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Float): scala.Float = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Double): scala.Double = value * x

  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Byte): scala.Int = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Short): scala.Int = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Char): scala.Int = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Int): scala.Int = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Long): scala.Long = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Float): scala.Float = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Double): scala.Double = value / x

  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Byte): scala.Int = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Short): scala.Int = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Char): scala.Int = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Int): scala.Int = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Long): scala.Long = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Float): scala.Float = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Double): scala.Double = value % x

}

object Character {
  val TYPE = classOf[scala.Char]
  val MIN_VALUE: scala.Char = 0
  val MAX_VALUE: scala.Char = 0xff

  def valueOf(charValue: scala.Char) = new Character(charValue)

  val LOWERCASE_LETTER: scala.Byte = 0
  val UPPERCASE_LETTER: scala.Byte = 0
  val OTHER_LETTER: scala.Byte = 0
  val TITLECASE_LETTER: scala.Byte = 0
  val LETTER_NUMBER: scala.Byte = 0
  val COMBINING_SPACING_MARK: scala.Byte = 0
  val ENCLOSING_MARK: scala.Byte = 0
  val NON_SPACING_MARK: scala.Byte = 0
  val MODIFIER_LETTER: scala.Byte = 0
  val DECIMAL_DIGIT_NUMBER: scala.Byte = 0
  val SURROGATE: scala.Byte = 0

  val MIN_RADIX: scala.Int = 2
  val MAX_RADIX: scala.Int = 36

  val MIN_HIGH_SURROGATE: scala.Char = '\uD800'
  val MAX_HIGH_SURROGATE: scala.Char = '\uDBFF'
  val MIN_LOW_SURROGATE: scala.Char = '\uDC00'
  val MAX_LOW_SURROGATE: scala.Char = '\uDFFF'
  val MIN_SURROGATE: scala.Char = MIN_HIGH_SURROGATE
  val MAX_SURROGATE: scala.Char = MAX_LOW_SURROGATE

  /* Tests */
  def getType(ch: scala.Char): scala.Int = sys.error("unimplemented")
  def getType(codePoint: scala.Int): scala.Int = sys.error("unimplemented")
  def digit(c: scala.Char, radix: scala.Int): scala.Int = {
    if (radix > MAX_RADIX || radix < MIN_RADIX)
      -1
    else if (c >= '0' && c <= '9' && c - '0' < radix)
      c - '0'
    else if (c >= 'A' && c <= 'Z' && c - 'A' < radix - 10)
      c - 'A' + 10
    else if (c >= 'a' && c <= 'z' && c - 'a' < radix - 10)
      c - 'a' + 10
    else if (c >= '\uFF21' && c <= '\uFF3A' &&
      c - '\uFF21' < radix - 10)
      c - '\uFF21' + 10
    else if (c >= '\uFF41' && c <= '\uFF5A' &&
      c - '\uFF41' < radix - 10)
      c - '\uFF21' + 10
    else -1
  }

  def isISOControl(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isDigit(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isLetter(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isLetterOrDigit(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isWhitespace(c: scala.Char): scala.Boolean = js.RegExp("^\\s$").test(c.toString)
  def isSpaceChar(c: scala.Char): scala.Boolean = sys.error("unimplemented")

  def isHighSurrogate(c: scala.Char): scala.Boolean =
    (c >= MIN_HIGH_SURROGATE) && (c <= MAX_HIGH_SURROGATE)
  def isLowSurrogate(c: scala.Char): scala.Boolean =
    (c >= MIN_LOW_SURROGATE) && (c <= MAX_LOW_SURROGATE)
  def isSurrogatePair(high: scala.Char, low: scala.Char): scala.Boolean =
    isHighSurrogate(high) && isLowSurrogate(low)

  def isUnicodeIdentifierStart(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isUnicodeIdentifierPart(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isIdentifierIgnorable(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isMirrored(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isLowerCase(c: scala.Char): scala.Boolean = toLowerCase(c) == c
  def isUpperCase(c: scala.Char): scala.Boolean = toUpperCase(c) == c
  def isTitleCase(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isJavaIdentifierPart(c: scala.Char): scala.Boolean = sys.error("unimplemented")

  def getDirectionality(c: scala.Char): scala.Byte = sys.error("unimplemented")

  /* Conversions */
  def toUpperCase(c: scala.Char): scala.Char = c.toString.toUpperCase()(0)
  def toLowerCase(c: scala.Char): scala.Char = c.toString.toLowerCase()(0)
  def toTitleCase(c: scala.Char): scala.Char = sys.error("unimplemented")
  def getNumericValue(c: scala.Char): scala.Int = sys.error("unimplemented")

  /* Misc */
  def reverseBytes(ch: scala.Char): scala.Char = sys.error("unimplemented")

  def toString(c: scala.Char) = valueOf(c).toString
}
