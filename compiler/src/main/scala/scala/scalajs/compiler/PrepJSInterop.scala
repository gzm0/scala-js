/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author Tobias Schlatter
 */

package scala.scalajs.compiler

import scala.tools.nsc
import nsc._

/** Prepares classes extending js.Any for JavaScript interop
 *
 * This phase does two things:
 * - Annotate subclasses of js.Any to be treated specially
 * - Handle extension methods to subclasses of js.Any
 * 
 * @author Tobias Schlatter
 */
abstract class PrepJSInterop extends plugins.PluginComponent with transform.Transform {
  val jsAddons: JSGlobalAddons {
    val global: PrepJSInterop.this.global.type
  }

  import global._
  import jsAddons._
  import jsDefinitions._
  
  val phaseName = "jsinterop"

  override def newPhase(p: nsc.Phase) = new JSInteropPhase(p)
  class JSInteropPhase(prev: nsc.Phase) extends Phase(prev) {
    override def name = phaseName
    override def description = "Prepare ASTs for JavaScript interop"
  }

  override protected def newTransformer(unit: CompilationUnit) =
    new JSInteropTransformer(unit)

  class JSInteropTransformer(unit: CompilationUnit) extends Transformer {
    val cpy = treeCopy

    override def transform(tree: Tree): Tree = super.transform(tree) match {
      case idef: ImplDef if isJSAny(idef) =>
        transformImplDef(idef)
      case _ => tree
    }

    private def isJSAny(implDef: ImplDef) =
      implDef.symbol.tpe.typeSymbol isSubClass JSAnyClass

    private def transformImplDef(implDef: ImplDef) = {
      val sym = implDef.symbol
      sym.setAnnotations(rawJSAnnot :: sym.annotations)

      // TODO add extractor methods

      implDef
    }

    private def rawJSAnnot =
      Annotation(typeOf[scala.scalajs.js.annotation.RawJSType], Nil, Nil)
    
  }
  
}
