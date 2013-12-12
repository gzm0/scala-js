/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author Tobias Schlatter
 */

package scala.scalajs.compiler

import scala.tools.nsc._

/** Prepares classes extending js.Any for JavaScript interop
 *
 * This phase does two things:
 * - Annotate subclasses of js.Any to be treated specially
 * - Handle extension methods to subclasses of js.Any
 * 
 * @author Tobias Schlatter
 */
abstract class PrepJSInterop extends plugins.PluginComponent {
  val jsAddons: JSGlobalAddons {
    val global: PrepJSInterop.this.global.type
  }

  import global._
  import jsAddons._
  import jsDefinitions._
  
  val phaseName = "jsinterop"
    
  override def newPhase(p: Phase) = new JSInteropPhase(p)
    
  class JSInteropPhase(prev: Phase) extends StdPhase(prev) {
 
    override def name = phaseName
    override def description = "Prepare ASTs for JavaScript interop"

    override def apply(cunit: CompilationUnit): Unit =
      pass(cunit.body)
    
    /**
     * passes over the tree and extracts class and module definitions
     */
    private def pass(tree: Tree): Unit = tree match {
      case PackageDef(_, stats) => stats foreach pass
      case cd: ClassDef  => handleImplDef(cd)
      case md: ModuleDef => handleImplDef(md)
      // TODO be more restrictive here (avoid useless traversal)
      case tree: Tree =>
        for (t <- tree.children) pass(t)
    }

    private def handleImplDef(implDef: ImplDef) = {
      val tSym = implDef.symbol.tpe.typeSymbol

      if (tSym isSubClass JSAnyClass) {
        println(s"tagging $tSym")
        tSym.updateAttachment(ScalaJSPlugin.RawJSTypeTag)
      }

      // descend into class definition
      pass(implDef.impl)
    }
    
  }
  
}
