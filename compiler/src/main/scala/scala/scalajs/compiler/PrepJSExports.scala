/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Tobias Schlatter
 */

package scala.scalajs.compiler

/**
 *  Prepare export generation
 *
 *  Helpers for transformation of @JSExport annotations
 */
trait PrepJSExports { this: PrepJSInterop =>

  import global._
  import jsAddons._
  import definitions._
  import jsDefinitions._

  import scala.reflect.internal.Flags

  // TODO special case constructors here
  def genExportMember(ddef: DefDef): List[Tree] = {
    val baseSym = ddef.symbol
    val clsSym = baseSym.owner

    val exportNames = jsInterop.exportSpecsOf(baseSym)

    // Helper function for errors
    def err(msg: String) = { currentUnit.error(exportNames.head.pos, msg); Nil }

    if (exportNames.isEmpty)
      Nil
    else if (isJSAny(baseSym.owner))
      err("You may not export a method of a subclass of js.Any")
    else if (!baseSym.isPublic)
      err("You may not export a non-public member")
    else if (baseSym.isMacro)
      err("You may not export a macro")
    else if (scalaPrimitives.isPrimitive(baseSym))
      err("You may not export a primitive")
    else {
      assert(!baseSym.isBridge)

      // Reset interface flag: Any trait will contain non-empty methods
      clsSym.resetFlag(Flags.INTERFACE)

      lazy val propertyType =
        jsInterop.isSetterTpe(baseSym) ||
        jsInterop.isGetterTpe(baseSym)

      // Actually generate exporter methods
      for (spec @ jsInterop.ExportSpec(_, prop, pos) <- exportNames) yield {
        // Check that if we do a property, that we have the right type
        if (prop && !propertyType) {
          currentUnit.error(pos,
              s"""You cannot export ${baseSym.name} as a property, since it does not have an appropriate type.
                 |Acceptable types for properties are nullary method types (getters) and single argument, unit-return
                 |methods (setters)""".stripMargin)
          EmptyTree

        // If check is fine, actually generate tree
        } else atPos(pos) { genExportDef(baseSym, spec) }
      }
    }
  }

  /** generate an exporter for a DefDef */
  private def genExportDef(defSym: Symbol, expSpec: jsInterop.ExportSpec) = {
    val clsSym = defSym.owner
    val scalaName = jsInterop.scalaExportName(expSpec)

    // Create symbol for new method
    val expSym = defSym.cloneSymbol

    // Set position of symbol
    expSym.pos = expSpec.pos

    // Alter type for new method (lift return type to Any)
    // The return type is lifted, in order to avoid bridge
    // construction and to detect methods whose signature only differs
    // in the return type
    expSym.setInfo(retToAny(expSym.tpe))

    // Change name for new method
    expSym.name = scalaName

    // Update flags
    expSym.setFlag(Flags.SYNTHETIC)
    expSym.resetFlag(
        Flags.DEFERRED |  // We always have a body now
        Flags.OVERRIDE    // Synthetic methods need not bother with this
    )

    // Remove JSExport annotations
    expSym.removeAnnotation(JSExportAnnotation)

    // Add symbol to class
    clsSym.info.decls.enter(expSym)

    // Construct inner function call
    val sel: Tree = Select(This(clsSym), defSym)
    val rhs = (sel /: expSym.paramss) {
      (fun,params) => Apply(fun, params map Ident)
    }

    // Construct and type the actual tree
    typer.typedDefDef(DefDef(expSym, rhs))
  }

  /** changes the return type of the method type tpe to Any. returns new type */
  private def retToAny(tpe: Type): Type = tpe match {
    case MethodType(params, result) => MethodType(params, retToAny(result))
    case NullaryMethodType(result)  => NullaryMethodType(AnyClass.tpe)
    case PolyType(tparams, result)  => PolyType(tparams, retToAny(result))
    case _: TypeRef                 => AnyClass.tpe
    case _ => abort(s"Type of method is not method type, but ${tpe} of " +
        s"class ${tpe.getClass}")
  }



}