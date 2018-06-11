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

package org.scalajs.sbtplugin

import org.scalajs.logging._
import sbt.{Level => SbtLevel, Logger => SbtLogger}

import java.io._

object Loggers {
  private val lf = new PrintStream(new FileOutputStream("time.log", true), true)

  private class SbtLoggerWrapper(underlying: SbtLogger) extends Logger {
    def log(level: Level, message: => String): Unit =
      underlying.log(toolsLevel2sbtLevel(level), message)
    def trace(t: => Throwable): Unit =
      underlying.trace(t)
    override def time(title: String, nanos: Long): Unit = {
      super.time(title, nanos)
      lf.println(s"$title|$nanos")
    }
  }

  def sbtLogger2ToolsLogger(logger: SbtLogger): Logger =
    new SbtLoggerWrapper(logger)

  def sbtLevel2ToolsLevel(level: SbtLevel.Value): Level = level match {
    case SbtLevel.Error => Level.Error
    case SbtLevel.Warn  => Level.Warn
    case SbtLevel.Info  => Level.Info
    case SbtLevel.Debug => Level.Debug
  }

  def toolsLevel2sbtLevel(level: Level): SbtLevel.Value = level match {
    case Level.Error => SbtLevel.Error
    case Level.Warn  => SbtLevel.Warn
    case Level.Info  => SbtLevel.Info
    case Level.Debug => SbtLevel.Debug
  }
}
