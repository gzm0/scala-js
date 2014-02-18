/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._
import scala.math.PartialOrdering
import scala.reflect.internal.Flags


/** Generation of bridges for JavaScript
 *
 *  @author Sébastien Doeraene
 */
trait JSExports extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._
  import definitions._
  import jsDefinitions._
  import scalaPrimitives.isPrimitive

  // TODO these checks should be done in the prep interop phase
  /*
  private def isCandidateForExport(sym: Symbol): Boolean = (
     sym.isMethod      &&
    !sym.isBridge      &&
     sym.isPublic      &&
    !isPrimitive(sym)  &&
    !sym.isMacro)*/

  /** Marks an export: sym is exported with a given name
   *
   *  This can either be an obligation to export, or an already exported symbol
   */
  private case class Export(name: String, sym: Symbol, pos: Position) {
    /** whether this export fulfills a given obligation */
    def fulfills(e: Export) = {
      name == e.name && sym.name == e.sym.name && (
        sym.tpe <:< e.sym.tpe ||
        e.sym.allOverriddenSymbols.contains(sym)
      )
    }
    override def toString = s"Export(${sym.fullName} --> $name)"
  }

  private def localExports(sym: Symbol) = for {
    meth <- sym.tpe.decls
    (name, pos) <- jsExport.exportNamesOf(meth)
  } yield Export(name, meth, pos)

  private def exportBurdens(sym: Symbol) = for {
    iface <- sym.tpe.baseClasses
    if iface.isInterface
    exp   <- getExports(iface)
  } yield exp

  private def getExports(sym: Symbol): List[Export] = {
    if (sym == ObjectClass) Nil else {
      sym.attachments.get[List[Export]].getOrElse {
        val exps = if (sym.isInterface) {
          // If this is an interface, just create a simple burden list
          localExports(sym)
        } else {
          // Fetch all burdens from interfaces
          val burdens = exportBurdens(sym)
          // Fetch stuff from super
          val superExp = getExports(sym.superClass)
          // Fetch my stuff
          val thisExp = localExports(sym)
          burdens ++ superExp ++ thisExp
        }
        val res = exps.toList
        sym.updateAttachment(res)
        res
      }
    }
  }

  def genExportsForClass(sym: Symbol): List[js.Tree] = {
    val superExports = getExports(sym.superClass)
    val burdens = exportBurdens(sym)
    val localExps = localExports(sym)

    val exps = (localExps ++ burdens).filterNot(
        b => superExports.exists(_ fulfills b))

    //sym.tpe.

    /*println(s"Exports for ${sym.fullName}:")
    println(s" Burdens: ${burdens}")
    println(s" Super: ${superExports}")
    println(s" Local: ${localExps}")
    println(s" Final: ${exps}")*/
    Nil
  }
/*    val declaredExports = sym.info.decls.filter(isExported)
    val newlyDeclaredExports = declaredExports.filterNot(isOverridingBridge)
    val newlyDeclaredMethodNames =
      newlyDeclaredMethods.map(_.name.toTermName).toList.distinct
    newlyDeclaredMethodNames map (genBridge(sym, _))
    for { (name, altBuf) <- reg.exports }
      yield genExport(name, altBuf.toList)
  }.toList*/

  def genExport(name: String, alts: List[Symbol]) = {
    implicit val pos = alts.head.pos

    val altsByArgCount = alts.groupBy(_.tpe.params.size).toList.sortBy(_._1)

    val maxArgCount = altsByArgCount.last._1
    val formalsArgs = genFormalArgs(maxArgCount)

    val cases = for {
      (argc, methods) <- altsByArgCount
    } yield {
      (js.IntLiteral(argc), genBridgeSameArgc(methods, 0))
    }

    val body = {
      if (cases.size == 1) cases.head._2
      else {
        js.Switch(js.DotSelect(js.Ident("arguments"), js.Ident("length")),
            cases, genThrowTypeError())
      }
    }

    js.MethodDef(js.StringLiteral(name), formalsArgs, body)
  }

  /** checks if a symbol is overriding a symbol we already made a bridge for */
  private def isOverridingExport(sym: Symbol): Boolean = {
    lazy val osym = sym.nextOverriddenSymbol
    sym.isOverridingSymbol && osym.isPublic && !osym.owner.isInterface
  }
/*
  def genBridgesForClass(sym: Symbol): List[js.Tree] = {
    val declaredMethods = sym.info.decls.filter(isCandidateForBridge)
    val newlyDeclaredMethods = declaredMethods.filterNot(isOverridingBridge)
    val newlyDeclaredMethodNames =
      newlyDeclaredMethods.map(_.name.toTermName).toList.distinct
    newlyDeclaredMethodNames map (genBridge(sym, _))
  }

  private def genBridge(classSym: Symbol, name: TermName): js.Tree = {
    val alts0 = classSym.info.member(name).alternatives
    val alts1 = alts0.filter(isCandidateForBridge)
    val alts = alts1.filterNot(
        x => alts1.exists(y => (y ne x) && (y.tpe <:< x.tpe)))
    assert(!alts.isEmpty,
        s"Ended up with no alternatives for ${classSym.fullName}::$name. " +
        s"Original set was ${alts0} with types ${alts0.map(_.tpe)}")

    implicit val pos = alts.head.pos

    val altsByArgCount = alts.groupBy(_.tpe.params.size).toList.sortBy(_._1)

    val maxArgCount = altsByArgCount.last._1
    val formalsArgs = genFormalArgs(maxArgCount)

    val cases = for {
      (argc, methods) <- altsByArgCount
    } yield {
      (js.IntLiteral(argc), genBridgeSameArgc(methods, 0))
    }

    val body = {
      if (cases.size == 1) cases.head._2
      else {
        js.Switch(js.DotSelect(js.Ident("arguments"), js.Ident("length")),
            cases, genThrowTypeError())
      }
    }

    val jsName = name.toString match {
      case "<init>" => "init_" // will be stolen by the JS constructor
      case "constructor" => "$constructor"
      case x if x(0).isDigit    => "$" + x
      case x if x(0) == "$"     => "$" + x
      case x if js.isKeyword(x) => "$" + x
      case x => x
    }
    js.MethodDef(js.Ident(jsName), formalsArgs, body)
  }*/

  private def genBridgeSameArgc(alts: List[Symbol], paramIndex: Int): js.Tree = {
    implicit val pos = alts.head.pos

    val remainingParamLists = alts map (_.tpe.params.drop(paramIndex))

    if (alts.size == 1) genApplyForSym(alts.head)
    if (remainingParamLists.head.isEmpty) genTieBreak(alts)
    else {
      val altsByTypeTest = groupByWithoutHashCode(alts) {
        alt => typeTestForTpe(alt.tpe.params(paramIndex).tpe)
      }

      if (altsByTypeTest.size == 1) {
        // Testing this parameter is not doing any us good
        genBridgeSameArgc(alts, paramIndex+1)
      } else {
        // Sort them so that, e.g., isInstanceOf[String]
        // comes before isInstanceOf[Object]
        val sortedAltsByTypeTest = topoSortDistinctsBy(
            altsByTypeTest)(_._1)(RTTypeTest.Ordering)

        val defaultCase = genThrowTypeError()

        sortedAltsByTypeTest.foldRight[js.Tree](defaultCase) { (elem, elsep) =>
          val (typeTest, subAlts) = elem
          implicit val pos = subAlts.head.pos

          def param = genFormalArg(paramIndex+1)
          val genSubAlts = genBridgeSameArgc(subAlts, paramIndex+1)

          typeTest match {
            case TypeOfTypeTest(typeString) =>
              js.If(
                  js.BinaryOp("===", js.UnaryOp("typeof", param),
                      js.StringLiteral(typeString)),
                  genSubAlts, elsep)

            case InstanceOfTypeTest(tpe) =>
              js.If(genIsInstance(param, tpe), genSubAlts, elsep)

            case NoTypeTest =>
              genSubAlts // note: elsep is discarded, obviously
          }
        }
      }
    }
  }

  private def genIsInstance(value: js.Tree, tpe: Type)(
      implicit pos: Position): js.Tree = {
    encodeIsInstanceOf(value, tpe)
  }

  private def genTieBreak(alts: List[Symbol]): js.Tree = {
    // TODO For now we just emit the first one
    implicit val pos = alts.head.pos
    js.Block(genApplyForSym(alts.head))
  }

  private sealed abstract class RTTypeTest

  private final case class TypeOfTypeTest(typeString: String) extends RTTypeTest

  private final case class InstanceOfTypeTest(tpe: Type) extends RTTypeTest {
    override def equals(that: Any): Boolean = {
      that match {
        case InstanceOfTypeTest(thatTpe) => tpe =:= thatTpe
        case _ => false
      }
    }
  }

  private case object NoTypeTest extends RTTypeTest

  private object RTTypeTest {
    implicit object Ordering extends PartialOrdering[RTTypeTest] {
      override def tryCompare(lhs: RTTypeTest, rhs: RTTypeTest): Option[Int] = {
        if (lteq(lhs, rhs)) if (lteq(rhs, lhs)) Some(0) else Some(-1)
        else                if (lteq(rhs, lhs)) Some(1) else None
      }

      override def lteq(lhs: RTTypeTest, rhs: RTTypeTest): Boolean = {
        (lhs, rhs) match {
          case (_, NoTypeTest) => true
          case (NoTypeTest, _) => false

          case (TypeOfTypeTest(s1), TypeOfTypeTest(s2)) =>
            s1 <= s2

          case (InstanceOfTypeTest(t1), InstanceOfTypeTest(t2)) =>
            t1 <:< t2

          case (_:TypeOfTypeTest, _:InstanceOfTypeTest) => true
          case (_:InstanceOfTypeTest, _:TypeOfTypeTest) => false
        }
      }

      override def equiv(lhs: RTTypeTest, rhs: RTTypeTest): Boolean = {
        lhs == rhs
      }
    }
  }

  // Group-by that does not rely on hashCode(), only equals() - O(n²)
  private def groupByWithoutHashCode[A, B](
      coll: List[A])(f: A => B): List[(B, List[A])] = {

    import scala.collection.mutable.ArrayBuffer
    val m = new ArrayBuffer[(B, List[A])]
    m.sizeHint(coll.length)

    for (elem <- coll) {
      val key = f(elem)
      val index = m.indexWhere(_._1 == key)
      if (index < 0) m += ((key, List(elem)))
      else m(index) = (key, elem :: m(index)._2)
    }

    m.toList
  }

  // Very simple O(n²) topological sort for elements assumed to be distinct
  private def topoSortDistinctsBy[A <: AnyRef, B](coll: List[A])(f: A => B)(
      implicit ord: PartialOrdering[B]): List[A] = {

    @scala.annotation.tailrec
    def loop(coll: List[A], acc: List[A]): List[A] = {
      if (coll.isEmpty) acc
      else if (coll.tail.isEmpty) coll.head :: acc
      else {
        val (lhs, rhs) = coll.span(x => !coll.forall(
            y => (x eq y) || !ord.lteq(f(x), f(y))))
        assert(!rhs.isEmpty, s"cycle while ordering $coll")
        loop(lhs ::: rhs.tail, rhs.head :: acc)
      }
    }

    loop(coll, Nil)
  }

  private def typeTestForTpe(tpe: Type): RTTypeTest = {
    toTypeKind(tpe) match {
      case UNDEFINED => TypeOfTypeTest("undefined")
      case _:INT | _:FLOAT => TypeOfTypeTest("number")
      case BOOL => TypeOfTypeTest("boolean")

      case REFERENCE(cls) =>
        if (cls == StringClass) TypeOfTypeTest("string")
        else if (isRawJSType(tpe)) {
          cls match {
            case JSNumberClass => TypeOfTypeTest("number")
            case JSBooleanClass => TypeOfTypeTest("boolean")
            case JSStringClass => TypeOfTypeTest("string")
            case JSUndefinedClass => TypeOfTypeTest("undefined")
            case _ => NoTypeTest
          }
        } else InstanceOfTypeTest(tpe)

      case ARRAY(_) => InstanceOfTypeTest(tpe)
    }
  }

  private def genApplyForSym(sym: Symbol): js.Tree = {
    implicit val pos = sym.pos
    js.Return {
      js.ApplyMethod(js.This(), encodeMethodSym(sym),
          genFormalArgs(sym.tpe.params.size))
    }
  }

  private def genThrowTypeError()(
      implicit pos: Position): js.Tree = {
    js.Throw(js.StringLiteral("No matching overload"))
  }

  private def genFormalArgs(count: Int)(implicit pos: Position): List[js.Ident] =
    (1 to count map genFormalArg).toList

  private def genFormalArg(index: Int)(implicit pos: Position): js.Ident =
    js.Ident("arg$" + index)

  private def isExported(sym: Symbol) =
    sym.getAnnotation(JSExportAnnotation).isDefined

}
