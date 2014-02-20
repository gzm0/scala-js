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
  def genExportMember(member: ValOrDefDef): List[Tree] = member match {
    case ddef: DefDef => genExportSym(ddef.symbol)
    case vdef: ValDef =>
      // TODO this does currently not work, since getter and setter are not yet
      // defined
      val baseSym = vdef.symbol
      val getter = baseSym.getter(baseSym.owner)
      val setter = baseSym.setter(baseSym.owner)
      for {
        sym <- getter :: setter :: Nil
        if sym != NoSymbol
        exp <- genExportSym(sym)
      } yield exp
  }

  private def genExportSym(baseSym: Symbol) = {
    val clsSym = baseSym.owner
    val exportNames = jsExport.exportNamesOf(baseSym)

    // Position of one annotation for error messages
    def errorPos = exportNames.head._2

    if (exportNames.isEmpty || !checkExportAllowed(baseSym, errorPos)) {
      Nil
    } else {
      assert(!baseSym.isBridge)

      // Reset interface flag: Any trait will contain non-empty methods
      clsSym.resetFlag(Flags.INTERFACE)

      // Actually generate exporter methods
      for ((expName, pos) <- exportNames) yield atPos(pos) {
        val scalaName = jsExport.scalaExportName(expName)

        // Create symbol for new method
        val expSym = baseSym.cloneSymbol

        // Alter type for new method (lift return type to Any)
        // The return type is lifted, in order to avoid bridge
        // construction and to detect methods whose signature only differs
        // in the return type
        expSym.setInfo(retToAny(expSym.tpe))

        // Change name for new method
        expSym.name = scalaName

        // Update flags
        expSym.resetFlag(Flags.DEFERRED | Flags.OVERRIDE)
        expSym.setFlag(Flags.SYNTHETIC)

        // Remove JSExport annotations
        expSym.removeAnnotation(JSExportAnnotation)

        // Add symbol to class
        clsSym.info.decls.enter(expSym)

        // Construct inner function call
        val sel: Tree = Select(This(clsSym), baseSym)
        val rhs = (sel /: expSym.paramss) {
          (fun,params) => Apply(fun, params map Ident)
        }

        // Construct and type the actual tree
        typer.typedDefDef(DefDef(expSym, rhs))
      }
    }
  }

  private def checkExportAllowed(memSym: Symbol, pos: Position) = {
    if (isJSAny(memSym.owner)) {
      currentUnit.error(pos,
          "You may not export a method of a subclass of js.Any")
      false
    } else if (!memSym.isPublic) {
      currentUnit.error(pos, "You may not export a non-public member")
      false
    } else if (memSym.isMacro) {
      currentUnit.error(pos, "You may not export a macro")
      false
    } else if (scalaPrimitives.isPrimitive(memSym)) {
      currentUnit.error(pos, "You may not export a primitive")
      false
    } else true
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