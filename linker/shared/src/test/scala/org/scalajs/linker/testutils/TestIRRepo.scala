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

package org.scalajs.linker.testutils

import scala.concurrent._

import org.scalajs.linker.StandardImpl
import org.scalajs.linker.interface.IRFile

object TestIRRepo {
  private val globalIRCache = StandardImpl.irFileCache()

  val minilib: Future[Seq[IRFile]] = loadGlobal(StdlibHolder.minilib)
  val javalib: Future[Seq[IRFile]] = loadGlobal(StdlibHolder.javalib)
  val empty: Future[Seq[IRFile]] = Future.successful(Vector())

  private def loadGlobal(stdlibPath: String): Future[Seq[IRFile]] = {
    import scala.concurrent.ExecutionContext.Implicits.global

    Platform.loadJar(stdlibPath)
      .flatMap(globalIRCache.newCache.cached _)
  }

  /** For each previous lib, calls `f(version, irFiles)`, and combines the result.
   *
   *  This method applies `f` *sequentially*. It waits until the returned
   *  `Future` completes before moving on to the next iteration.
   */
  def sequentiallyForEachPreviousLib[A](f: (String, Seq[IRFile]) => Future[A])(
      implicit ec: ExecutionContext): Future[Vector[A]] = {

    // sort for determinism
    val sortedPreviousLibs = StdlibHolder.previousLibs.toVector.sortBy(_._1)

    sequentialFutureTraverse(sortedPreviousLibs) { case (version, path) =>
      Platform.loadJar(path).flatMap { files =>
        val cache = globalIRCache.newCache
        cache
          .cached(files)
          .flatMap(f(version, _))
          .andThen { case _ => cache.free() }
      }
    }
  }

  /** Like `Future.traverse`, but waits until each `Future` has completed
   *  before starting the next one.
   */
  private def sequentialFutureTraverse[A, B](items: Vector[A])(f: A => Future[B])(
      implicit ec: ExecutionContext): Future[Vector[B]] = {
    items match {
      case Vector() =>
        Future.successful(Vector())
      case head +: tail =>
        for {
          headResult <- f(head)
          tailResult <- sequentialFutureTraverse(tail)(f)
        } yield {
          headResult +: tailResult
        }
    }
  }
}
