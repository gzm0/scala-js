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

package org.scalajs.ir

import java.io._

object TreeInstrumentation {
  private val csvPath = "/home/tos/gh/scala-js/tree-arg-sizes.csv"

  private val writer: PrintWriter = {
    val file = new java.io.File(csvPath)
    val needsHeader = !file.exists() || file.length() == 0L
    val pw = new PrintWriter(new BufferedWriter(new FileWriter(file)))
    pw.println("tree,field,size")
    Runtime.getRuntime().addShutdownHook(new Thread(() => pw.close()))
    pw
  }

  def record(tree: String, field: String, size: Int): Unit = synchronized {
    writer.println(s"$tree,$field,$size")
  }
}
