/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.backend.javascript

import java.io._
import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.{util => ju}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

import org.scalajs.ir
import org.scalajs.ir.OriginalName
import org.scalajs.ir.Position
import org.scalajs.ir.Position._

object SourceMapWriter {
  private val Base64UpperMap: Array[Byte] =
    "ghijklmnopqrstuvwxyz0123456789+/".toArray.map(_.toByte)

  // Some constants for writeBase64VLQ
  // Each base-64 digit covers 6 bits, but 1 is used for the continuation
  private final val VLQBaseShift = 5
  private final val VLQBase = 1 << VLQBaseShift
  private final val VLQBaseMask = VLQBase - 1
  private final val VLQContinuationBit = VLQBase

  private final class NodePosStack {
    private var topIndex: Int = -1
    private var posStack: Array[Position] = new Array(128)
    private var nameStack: Array[String] = new Array(128)

    def pop(): Unit =
      topIndex -= 1

    def topPos: Position =
      posStack(topIndex)

    def topName: String =
      nameStack(topIndex)

    def push(pos: Position, originalName: String): Unit = {
      val newTopIdx = topIndex + 1
      topIndex = newTopIdx
      if (newTopIdx >= posStack.length)
        growStack()
      posStack(newTopIdx) = pos
      nameStack(newTopIdx) = originalName
    }

    private def growStack(): Unit = {
      val newSize = 2 * posStack.length
      posStack = ju.Arrays.copyOf(posStack, newSize)
      nameStack = ju.Arrays.copyOf(nameStack, newSize)
    }
  }

  final class Fragment private[SourceMapWriter] (
    private[SourceMapWriter] val data: Array[Byte]
  ) extends AnyVal

  object Fragment {
    val Empty: Fragment = new Fragment(new Array(0))
  }

  sealed abstract class Builder(
    protected val out: ByteArrayWriter,
    protected val outIndex: Index,
    protected val fragmentIndex: Index,
  ) {
    // Strings are nullable in this stack
    private val nodePosStack = new SourceMapWriter.NodePosStack
    nodePosStack.push(NoPosition, null)

    private var pendingColumnInGenerated: Int = -1
    private var pendingPos: Position = NoPosition
    private var pendingIsIdent: Boolean = false
    // pendingName string is nullable
    private var pendingName: String = null

    private var lineCountInGenerated = 0

    private var lastColumnInGenerated = 0
    private var firstSegmentOfLine = true
    private var lastSource: SourceFile = null
    private var lastSourceIndex = 0
    private var lastLine: Int = 0
    private var lastColumn: Int = 0
    private var lastNameIndex: Int = 0

    final def nextLine(): Unit = {
      writePendingSegment()
      doWriteNewLine()
      pendingColumnInGenerated = -1
      pendingPos = nodePosStack.topPos
      pendingName = nodePosStack.topName
    }

    final def startNode(column: Int, originalPos: Position): Unit = {
      nodePosStack.push(originalPos, null)
      startSegment(column, originalPos, isIdent = false, null)
    }

    final def startIdentNode(column: Int, originalPos: Position,
        optOriginalName: OriginalName): Unit = {
      // TODO The then branch allocates a String; we should avoid that at some point
      val originalName =
        if (optOriginalName.isDefined) optOriginalName.get.toString()
        else null
      nodePosStack.push(originalPos, originalName)
      startSegment(column, originalPos, isIdent = true, originalName)
    }

    final def endNode(column: Int): Unit = {
      nodePosStack.pop()
      startSegment(column, nodePosStack.topPos, isIdent = false,
          nodePosStack.topName)
    }

    final def insertFragment(fragment: Fragment): Unit = {
      require(pendingColumnInGenerated < 0, s"Cannot add fragment when in the middle of a line")

      val buf = ByteBuffer.wrap(fragment.data)

      var columnInGenerated = 0
      var sourceIndex = 0
      var line: Int = 0
      var column: Int = 0
      var nameIndex: Int = 0

      var nextByte: Byte = 0

      while (buf.hasRemaining()) {
        nextByte = buf.get()

        if (nextByte != ';') {
          columnInGenerated += readBase64VLQ(nextByte, buf)

          nextByte = buf.get()

          if (nextByte == ',' || nextByte == ';') {
            doWriteSegment(columnInGenerated)
          } else {
            sourceIndex += readBase64VLQ(nextByte, buf)
            line += readBase64VLQ(buf)
            column += readBase64VLQ(buf)

            val source = fragmentIndex.sources(sourceIndex)

            nextByte = buf.get()

            if (nextByte == ',' || nextByte == ';') {
              doWriteSegment(columnInGenerated, source, line, column, null)
            } else {
              nameIndex += readBase64VLQ(nextByte, buf)
              val name = fragmentIndex.names(nameIndex)
              doWriteSegment(columnInGenerated, source, line, column, name)

              nextByte = buf.get()
              assert(nextByte == ',' || nextByte == ';', s"bad next byte, got: $nextByte")
            }
          }
        }

        if (nextByte == ';') {
          doWriteNewLine()
          columnInGenerated = 0
        }
      }
    }

    final def complete(): Unit = {
      if (!firstSegmentOfLine)
        throw new IllegalStateException("Trying to complete a fragment in the middle of a line")

      doComplete(lineCountInGenerated)
    }

    private def startSegment(startColumn: Int, originalPos: Position,
        isIdent: Boolean, originalName: String): Unit = {
      // scalastyle:off return

      // There is no point in outputting a segment with the same information
      if ((originalPos == pendingPos) && (isIdent == pendingIsIdent) &&
          (originalName == pendingName)) {
        return
      }

      // Write pending segment if it covers a non-empty range
      if (startColumn != pendingColumnInGenerated)
        writePendingSegment()

      // New pending
      pendingColumnInGenerated = startColumn
      pendingPos = originalPos
      pendingIsIdent = isIdent
      pendingName = originalName

      // scalastyle:on return
    }

    private def writePendingSegment(): Unit = {
      if (pendingColumnInGenerated >= 0)
        doWriteSegment(pendingColumnInGenerated, pendingPos, pendingName)
    }

    private def doWriteNewLine(): Unit = {
      out.write(';')
      lineCountInGenerated += 1
      lastColumnInGenerated = 0
      firstSegmentOfLine = true
    }

    private def doWriteSegment(columnInGenerated: Int, pos: Position, name: String): Unit = {
      if (pos.isEmpty) doWriteSegment(columnInGenerated)
      else doWriteSegment(columnInGenerated, pos.source, pos.line, pos.column, name)
    }

    private def doWriteSegment(columnInGenerated: Int): Unit = {
      /* This method is incredibly performance-sensitive, so we resort to
       * "unsafe" direct access to the underlying array of `out`.
       */
      val MaxSegmentLength = 1 + 7 // ',' + 1 base64VLQ of max 7 bytes each
      val buffer = out.unsafeStartDirectWrite(maxBytes = MaxSegmentLength)
      var offset = out.currentSize

      // Segments of a line are separated by ','
      if (firstSegmentOfLine) {
        firstSegmentOfLine = false
      } else {
        buffer(offset) = ','
        offset += 1
      }

      // Generated column field
      offset = writeBase64VLQ(buffer, offset, columnInGenerated-lastColumnInGenerated)
      lastColumnInGenerated = columnInGenerated

      out.unsafeEndDirectWrite(offset)
    }

    private def doWriteSegment(columnInGenerated: Int, source: SourceFile, line: Int, column: Int, name: String): Unit = {
      // scalastyle:off return

      /* This method is incredibly performance-sensitive, so we resort to
       * "unsafe" direct access to the underlying array of `out`.
       */
      val MaxSegmentLength = 1 + 5 * 7 // ',' + max 5 base64VLQ of max 7 bytes each
      val buffer = out.unsafeStartDirectWrite(maxBytes = MaxSegmentLength)
      var offset = out.currentSize

      // Segments of a line are separated by ','
      if (firstSegmentOfLine) {
        firstSegmentOfLine = false
      } else {
        buffer(offset) = ','
        offset += 1
      }

      // Generated column field
      offset = writeBase64VLQ(buffer, offset, columnInGenerated-lastColumnInGenerated)
      lastColumnInGenerated = columnInGenerated

      // Source index field
      if (source eq lastSource) { // highly likely
        buffer(offset) = 'A' // 0 in Base64VLQ
        offset += 1
      } else {
        val sourceIndex = outIndex.sourceToIndex(source)
        offset = writeBase64VLQ(buffer, offset, sourceIndex-lastSourceIndex)
        lastSource = source
        lastSourceIndex = sourceIndex
      }

      // Line field
      offset = writeBase64VLQ(buffer, offset, line - lastLine)
      lastLine = line

      // Column field
      offset = writeBase64VLQ(buffer, offset, column - lastColumn)
      lastColumn = column

      // Name field
      if (name != null) {
        val nameIndex = outIndex.nameToIndex(name)
        offset = writeBase64VLQ(buffer, offset, nameIndex-lastNameIndex)
        lastNameIndex = nameIndex
      }

      out.unsafeEndDirectWrite(offset)

      // scalastyle:on return
    }

    protected def doComplete(lineCount: Int): Unit

    /** Write the Base 64 VLQ of an integer to the mappings.
     *
     *  !!! This method is surprisingly performance-sensitive. In an incremental
     *  run of the linker, it takes half of the time of the `BasicLinkerBackend`
     *  and systematically shows up on performance profiles. If you change it,
     *  profile it and measure performance of source map generation.
     *
     *  @return
     *    the offset past the written bytes in the `buffer`, i.e., `offset + x`
     *    where `x` is the amount of bytes written
     */
    private def writeBase64VLQ(buffer: Array[Byte], offset: Int, value0: Int): Int = {
      /* The sign is encoded in the least significant bit, while the
       * absolute value is shifted one bit to the left.
       * So in theory the "definition" of `value` is:
       *   val value =
       *     if (value0 < 0) ((-value0) << 1) | 1
       *     else value0 << 1
       * The following code is a branchless version of that spec.
       * It is valid because:
       * - if value0 < 0:
       *   signExtended == value0 >> 31 == 0xffffffff
       *   value0 ^ signExtended == ~value0
       *   (value0 ^ signExtended) - signExtended == ~value0 - (-1) == -value0
       *   signExtended & 1 == 1
       *   So we get ((-value0) << 1) | 1 as required
       * - if n >= 0:
       *   signExtended == value0 >> 31 == 0
       *   value0 ^ signExtended == value0
       *   (value0 ^ signExtended) - signExtended == value0 - 0 == value0
       *   signExtended & 1 == 0
       *   So we get (value0 << 1) | 0 == value0 << 1 as required
       */
      val signExtended = value0 >> 31
      val value = (((value0 ^ signExtended) - signExtended) << 1) | (signExtended & 1)

      /* Now that we have a non-negative `value`, we encode it in base64 by
       * blocks of 5 bits. Each base64 digit stores 6 bits, but the most
       * significant one is used as a continuation bit (1 to continue, 0 to
       * indicate the last block). The payload is stored in little endian, with
       * the least significant blocks first.
       *
       * We could use a unique lookup table for the 64 base64 digits. However,
       * since in every path we either always pick in the lower half (for the
       * last byte) or the upper half (for continuation bytes), we use two
       * distinct functions, and omit the implicit `| VLQContinuationBit` in the
       * upper half.
       *
       * The upper half, in `continuationByte`, actually uses a lookup table.
       *
       * The lower half, in `lastByte`, uses a branchless, memory access-free
       * algorithm. The logical way to write it would be
       *   if (v < 26) v + 'A' else (v - 26) + 'a'
       * Because 'a' == 'A' + 32, this is equivalent to
       *   if (v < 26) v + 'A' else v - 26 + 'A' + 32
       * Factoring out v + 'A' and adding constants, we get
       *   v + 'A' + (if (v < 26) 0 else 6)
       * We rewrite the condition as the following branchless algorithm:
       *   ((25 - v) >> 31) & 6
       * It is equivalent because:
       *   * (25 - v) is < 0 iff v >= 26
       *   * i.e., its sign bit is 1 iff v >= 26
       *   * (25 - v) >> 31 is all-1's if v >= 26, and all-0's if v < 26
       *   * ((25 - v) >> 31) & 6 is 6 if v >= 26, and 0 if v < 26
       * This gives us the algorithm used in `lastByte`:
       *   v + 'A' + (((25 - v) >> 31) & 6)
       *
       * Compared to the lookup table, this seems to exhibit a 5-10% speedup for
       * the source map generation.
       */

      // Precondition: 0 <= v < 32, i.e., (v & 31) == v
      def continuationByte(v: Int): Byte =
        Base64UpperMap(v)

      // Precondition: 0 <= v < 32, i.e., (v & 31) == v
      def lastByte(v: Int): Byte =
        (v + 'A' + (((25 - v) >> 31) & 6)).toByte

      // Write as many base-64 digits as necessary to encode `value`
      if ((value & ~31) == 0) {
        // fast path for value < 32 -- store as a single byte (about 7/8 of the time for the test suite)
        buffer(offset) = lastByte(value)
        offset + 1
      } else if ((value & ~1023) == 0) {
        // fast path for 32 <= value < 1024 -- store as two bytes (about 1/8 of the time for the test suite)
        buffer(offset) = continuationByte(value & VLQBaseMask)
        buffer(offset + 1) = lastByte(value >>> 5)
        offset + 2
      } else {
        // slow path for 1024 <= value -- store as 3 bytes or more (a negligible fraction of the time)
        def writeBase64VLQSlowPath(value0: Int): Int = {
          var offset1 = offset
          var value = value0
          var digit = 0
          while ({
            digit = value & VLQBaseMask
            value = value >>> VLQBaseShift
            value != 0
          }) {
            buffer(offset1) = continuationByte(digit)
            offset1 += 1
          }
          buffer(offset1) = lastByte(digit)
          offset1 + 1
        }
        writeBase64VLQSlowPath(value)
      }
    }

    private def readBase64VLQ(buf: ByteBuffer): Int =
      readBase64VLQ(buf.get(), buf)

    private def readBase64VLQ(first: Byte, buf: ByteBuffer): Int = {
      var i = base64ToInt(first)
      var shift = 0
      var value = i & VLQBaseMask

      while ((i & VLQContinuationBit) != 0) {
        shift += 5
        i = base64ToInt(buf.get())
        value += (i & VLQBaseMask) << shift
      }

      val neg = (value & 1) != 0

      value >>>= 1

      // TODO: Is neg of 0 min int?
      if (neg) -value else value
    }

    private def base64ToInt(x: Byte): Int = x match {
      case 'A' => 0
      case 'B' => 1
      case 'C' => 2
      case 'D' => 3
      case 'E' => 4
      case 'F' => 5
      case 'G' => 6
      case 'H' => 7
      case 'I' => 8
      case 'J' => 9
      case 'K' => 10
      case 'L' => 11
      case 'M' => 12
      case 'N' => 13
      case 'O' => 14
      case 'P' => 15
      case 'Q' => 16
      case 'R' => 17
      case 'S' => 18
      case 'T' => 19
      case 'U' => 20
      case 'V' => 21
      case 'W' => 22
      case 'X' => 23
      case 'Y' => 24
      case 'Z' => 25
      case 'a' => 26
      case 'b' => 27
      case 'c' => 28
      case 'd' => 29
      case 'e' => 30
      case 'f' => 31
      case 'g' => 32
      case 'h' => 33
      case 'i' => 34
      case 'j' => 35
      case 'k' => 36
      case 'l' => 37
      case 'm' => 38
      case 'n' => 39
      case 'o' => 40
      case 'p' => 41
      case 'q' => 42
      case 'r' => 43
      case 's' => 44
      case 't' => 45
      case 'u' => 46
      case 'v' => 47
      case 'w' => 48
      case 'x' => 49
      case 'y' => 50
      case 'z' => 51
      case '0' => 52
      case '1' => 53
      case '2' => 54
      case '3' => 55
      case '4' => 56
      case '5' => 57
      case '6' => 58
      case '7' => 59
      case '8' => 60
      case '9' => 61
      case '+' => 62
      case '/' => 63
    }
  }

  final class FragmentBuilder(fragmentIndex: Index)
      extends Builder(new ByteArrayWriter, fragmentIndex, fragmentIndex) {

    protected def doComplete(lineCount: Int): Unit = ()

    def result(): Fragment = new Fragment(out.toByteArray)
  }

  final class Index {
    private[SourceMapWriter] val sources = new ArrayBuffer[SourceFile]
    private val _srcToIndex = new ju.HashMap[SourceFile, Integer]

    private[SourceMapWriter] val names = new ArrayBuffer[String]
    private val _nameToIndex = new ju.HashMap[String, Integer]

    private[SourceMapWriter] def sourceToIndex(source: SourceFile): Int = {
      val existing = _srcToIndex.get(source)
      if (existing != null) {
        existing.intValue()
      } else {
        val index = sources.size
        _srcToIndex.put(source, index)
        sources += source
        index
      }
    }

    private[SourceMapWriter] def nameToIndex(name: String): Int = {
      val existing = _nameToIndex.get(name)
      if (existing != null) {
        existing.intValue()
      } else {
        val index = names.size
        _nameToIndex.put(name, index)
        names += name
        index
      }
    }
  }
}

final class SourceMapWriter(out: ByteArrayWriter, jsFileName: String,
    relativizeBaseURI: Option[URI], fragmentIndex: SourceMapWriter.Index)
    extends SourceMapWriter.Builder(out, outIndex = new SourceMapWriter.Index, fragmentIndex = fragmentIndex) {

  import SourceMapWriter._

  writeHeader()

  private def writeJSONString(s: String): Unit = {
    out.write('\"')
    out.writeASCIIEscapedJSString(s)
    out.write('\"')
  }

  private def writeHeader(): Unit = {
    out.writeASCIIString("{\n\"version\": 3")
    out.writeASCIIString(",\n\"file\": ")
    writeJSONString(jsFileName)
    out.writeASCIIString(",\n\"mappings\": \"")
  }

  protected def doComplete(lineCount: Int): Unit = {
    val relativizeBaseURI = this.relativizeBaseURI // local copy
    var restSources = outIndex.sources.result() // TODO: Faster?
    out.writeASCIIString("\",\n\"sources\": [")
    while (restSources.nonEmpty) {
      writeJSONString(SourceFileUtil.webURI(relativizeBaseURI, restSources.head))
      restSources = restSources.tail
      if (restSources.nonEmpty)
        out.writeASCIIString(", ")
    }

    var restNames = outIndex.names.result() // TODO: Faster?
    out.writeASCIIString("],\n\"names\": [")
    while (restNames.nonEmpty) {
      writeJSONString(restNames.head)
      restNames = restNames.tail
      if (restNames.nonEmpty)
        out.writeASCIIString(", ")
    }
    out.writeASCIIString("],\n\"lineCount\": ")
    out.writeASCIIString(lineCount.toString)
    out.writeASCIIString("\n}\n")
  }
}
