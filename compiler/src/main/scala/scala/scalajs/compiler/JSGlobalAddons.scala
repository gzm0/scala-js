/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._

/** Additions to Global meaningful for the JavaScript backend
 *
 *  @author Sébastien Doeraene
 */
trait JSGlobalAddons extends JSTrees
                        with JSPrinters
                        with JSDefinitions
                        with JSTreeExtractors
                        with Compat210Component {
  val global: Global

  /** JavaScript primitives, used in jscode */
  object jsPrimitives extends JSPrimitives {
    val global: JSGlobalAddons.this.global.type = JSGlobalAddons.this.global
    val jsAddons: ThisJSGlobalAddons =
      JSGlobalAddons.this.asInstanceOf[ThisJSGlobalAddons]
  }

  /** global JSExport related helpers */
  object jsExport {
    import global._
    import jsDefinitions._

    private val exportPrefix = "$js$exported$"

    /** retrieves the names a sym should be exported to from its annotations */
    def exportNamesOf(sym: Symbol): List[(String, Position)] = for {
      annot <- sym.annotations
      if annot.symbol == JSExportAnnotation
      name = annot.stringArg(0).getOrElse(sym.unexpandedName.encoded)
      pos  = annot.pos
    } yield (name, pos)

    /** mangles a name to a JSExported name */
    def scalaExportName(name: String): Name = newTermName(exportPrefix + name)

    /** checks if the given name is a JSExport */
    def isExportName(name: Name): Boolean = name.startsWith(exportPrefix)

  }

}
