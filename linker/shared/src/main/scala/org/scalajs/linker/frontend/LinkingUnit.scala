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

package org.scalajs.linker.frontend

import org.scalajs.linker.interface.ModuleInitializer

import org.scalajs.linker.standard._

final class LinkingUnit private[frontend] (
    val classDefs: Vector[LinkedClass],
    val topLevelExports: Vector[LinkedTopLevelExport],
    val moduleInitializers: Vector[ModuleInitializer],
    val globalInfo: LinkedGlobalInfo
)
