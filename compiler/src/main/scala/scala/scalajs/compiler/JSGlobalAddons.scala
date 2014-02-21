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

  /** global javascript interop related helpers */
  object jsInterop {
    import global._
    import jsDefinitions._
    import definitions._

    import scala.reflect.NameTransformer

    private val exportPrefix =  "$js$exported$"
    private val methodExportPrefix = exportPrefix + "meth$"
    private val propExportPrefix = exportPrefix + "prop$"

    case class ExportSpec(name: String, prop: Boolean, pos: Position)

    /** retrieves the names a sym should be exported to from its annotations
     *
     *  Note that for accessor symbols, the annotations of the accessed symbol
     *  are used, rather than the annotations of the accessor itself.
     */
    def exportSpecsOf(sym: Symbol): List[ExportSpec] = for {
      annot <- (if (sym.isAccessor) sym.accessed else sym).annotations
      if annot.symbol == JSExportAnnotation
    } yield {
      val prop = annot.stringArg(1).map(_.toBoolean).getOrElse(isJSGetOrSet(sym))
      val name = annot.stringArg(0).getOrElse {
        val decN = sym.unexpandedName.decoded
        if (prop && isJSSetterTpe(sym)) {
          if (!decN.endsWith("_=")) decN.substring(0, decN.length - 2)
          else decN
        } else sym.unexpandedName.decoded
      }
      ExportSpec(name, prop, annot.pos)
    }

    /** creates a name for an export specification */
    def scalaExportName(spec: ExportSpec): Name = {
      val pref = if (spec.prop) propExportPrefix else methodExportPrefix
      val encname = NameTransformer.encode(spec.name)
      newTermName(pref + encname)
    }

    /** checks if the given name is a JSExport */
    def isExportName(name: Name): Boolean = name.startsWith(exportPrefix)

    /** checks if the given symbol is a JSExport */
    def isExport(sym: Symbol): Boolean = isExportName(sym.unexpandedName)

    /** retrieves the originally assigned JS name of this export */
    def jsExportName(name: Name): String = {
      assert(isExportName(name))
      name.encoded.substring(exportPrefix.length)
    }

    def isJSGetOrSet(sym: Symbol): Boolean = isJSGetter(sym) || isJSSetter(sym)

    /** has this symbol to be translated into a JS getter (both directions)? */
    def isJSGetter(sym: Symbol): Boolean = {
      sym.tpe.params.isEmpty && enteringPhase(currentRun.uncurryPhase) {
        sym.tpe.isInstanceOf[NullaryMethodType]
      }
    }

    /** has this symbol to be translated into a JS setter (both directions)? */
    def isJSSetter(sym: Symbol) = {
      sym.unexpandedName.decoded.endsWith("_=") &&
      isJSSetterTpe(sym)
    }

    private def isJSSetterTpe(sym: Symbol) = {
      enteringPhase(currentRun.uncurryPhase) {
        sym.tpe.paramss match {
          case List(List(arg)) => !isScalaRepeatedParamType(arg.tpe)
          case _ => false
        }
      }
    }

    /** has this symbol to be translated into a JS bracket access (JS to Scala) */
    def isJSBracketAccess(sym: Symbol) =
      sym.hasAnnotation(JSBracketAccessAnnotation)

  }

}
