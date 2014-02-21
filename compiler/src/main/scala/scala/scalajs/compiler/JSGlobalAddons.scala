/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._
import scala.reflect.internal.util.NoPosition

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
        if (prop && isSetterTpe(sym) && decN.endsWith("_="))
          decN.substring(0, decN.length - 2)
        else decN
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

    /** retrieves the originally assigned export spec of this export. note that
     *  it will have an invalid position
     */
    def jsExportSpec(name: Name): ExportSpec = {
      def dropPrefix(prefix: String) ={
        if (name.startsWith(prefix)) {
          // We can't encode right away due to $ separators
          val enc = name.encoded.substring(prefix.length)
          Some(NameTransformer.decode(enc))
        } else None
      }

      def expSpec(prop: Boolean) =
        (name: String) => ExportSpec(name, prop, global.NoPosition)

      dropPrefix(methodExportPrefix).map(expSpec(false)) orElse
      dropPrefix(propExportPrefix).map(expSpec(true)) getOrElse
      sys.error("non-exported name passed to jsExportSpec")
    }

    def isJSGetOrSet(sym: Symbol): Boolean = isJSGetter(sym) || isJSSetter(sym)

    /** has this symbol to be translated into a JS getter (both directions)? */
    def isJSGetter(sym: Symbol): Boolean = {
      sym.tpe.params.isEmpty && isGetterTpe(sym)
    }

    /** has this symbol to be translated into a JS setter (both directions)? */
    def isJSSetter(sym: Symbol) = {
      sym.unexpandedName.decoded.endsWith("_=") &&
      isSetterTpe(sym)
    }

    /** is this symbol a NullaryMethod */
    def isGetterTpe(sym: Symbol) = enteringPhase(currentRun.uncurryPhase) {
      sym.tpe.isInstanceOf[NullaryMethodType]
    }

    /** is this symbol a method with a single argument and Unit return type */
    def isSetterTpe(sym: Symbol) = {
      sym.tpe.resultType.typeSymbol == UnitClass &&
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
