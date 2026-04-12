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

package org.scalajs.linker

import scala.concurrent._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.ClassKind
import org.scalajs.ir.EntryPointsInfo
import org.scalajs.ir.Names._
import org.scalajs.ir.Traversers.Traverser
import org.scalajs.ir.Trees._
import org.scalajs.ir.Trees.MemberNamespace._
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.logging._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.IRAssertions._
import org.scalajs.linker.testutils.LinkingUtils._
import org.scalajs.linker.testutils.TestIRBuilder._

class OptimizerTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  import OptimizerTest._

  /** Generic code for the three methods below.
   *
   *  Check that `clone()` is correctly inlined (or correctly *not* inlined)
   *  when the receiver can be an array, in several scenarios.
   *
   *  Correct outcomes are:
   *  - The call is not inlined at all, or
   *  - It inlines `java.lang.Object.clone()`, which results in a `Clone` node.
   *
   *  A potential incorrect outcome, which has happened in the past (#3778), is
   *  to inline a method `clone()` from *another* class (for example, because
   *  `j.l.Object.clone()` is not otherwise reachable).
   */
  private def testCloneOnArrayInliningGeneric(inlinedWhenOnObject: Boolean,
      customMethodDefs: Vector[MethodDef]): Future[Unit] = {

    val thisFoo = thisFor("Foo")
    val intArrayTypeRef = ArrayTypeRef(IntRef, 1)
    val intArrayType = ArrayType(intArrayTypeRef, nullable = true, exact = false)
    val anArrayOfInts = ArrayValue(intArrayTypeRef, Vector(IntLiteral(1)))
    val newFoo = New("Foo", NoArgConstructorName, Vector())

    val reachCloneMethodName = m("reachClone", Vector(), O)
    val anArrayMethodName = m("anArray", Vector(), intArrayTypeRef)
    val anObjectMethodName = m("anObject", Vector(), O)

    def callCloneOn(receiver: Tree): Tree =
      consoleLog(Apply(EAF, receiver, cloneMethodName, Vector())(AnyType))

    val fooMethodDefs = Vector(
      trivialCtor("Foo"),

      // @noinline def witness(): AnyRef = throw null
      MethodDef(EMF, witnessMethodName, NON, Vector(), AnyType, Some {
        UnaryOp(UnaryOp.Throw, Null())
      })(EOH.withNoinline(true), UNV),

      // @noinline def reachClone(): Object = clone()
      MethodDef(EMF, reachCloneMethodName, NON, Vector(), AnyType, Some {
        Apply(EAF, thisFoo, cloneMethodName, Vector())(AnyType)
      })(EOH.withNoinline(true), UNV),

      // @noinline def anArray(): Array[Int] = Array(1)
      MethodDef(EMF, anArrayMethodName, NON, Vector(), intArrayType, Some {
        anArrayOfInts
      })(EOH.withNoinline(true), UNV),

      // @noinline def anObject(): AnyRef = Array(1)
      MethodDef(EMF, anObjectMethodName, NON, Vector(), AnyType, Some {
        anArrayOfInts
      })(EOH.withNoinline(true), UNV)
    ) ++ customMethodDefs

    val classDefs = Seq(
      classDef(
        "Foo",
        superClass = Some(ObjectClass),
        interfaces = Vector("java.lang.Cloneable"),
        methods = fooMethodDefs
      ),
      mainTestClassDef(Block(
        // new Foo().reachClone() -- make Foo.clone() reachable for sure
        Apply(EAF, newFoo, reachCloneMethodName, Vector())(AnyType),
        // Array(1).clone() -- test with an exact static type of I[] -> inline
        callCloneOn(anArrayOfInts),
        // new Foo().anArray().clone() -- test with a static type of I[] -> inline
        callCloneOn(Apply(EAF, newFoo, anArrayMethodName, Vector())(intArrayType)),
        // new Foo().anObject().clone() -- test with a static type of Object -> inlinedWhenOnObject
        callCloneOn(Apply(EAF, newFoo, anObjectMethodName, Vector())(AnyType))
      ))
    )

    for (moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)) yield {
      val linkedClass = moduleSet.modules.flatMap(_.classDefs)
        .find(_.className == MainTestClassName).get
      linkedClass.hasNot("any call to Foo.witness()") {
        case Apply(_, receiver, MethodIdent(`witnessMethodName`), _) =>
          receiver.tpe match {
            case ClassType(cls, _, _) => cls == ClassName("Foo")
            case _                    => false
          }
      }.hasExactly(if (inlinedWhenOnObject) 1 else 0, "IsInstanceOf node") {
        case IsInstanceOf(_, _) => true
      }.hasExactly(if (inlinedWhenOnObject) 1 else 0, "throw operation") {
        case UnaryOp(UnaryOp.Throw, _) => true
      }.hasExactly(if (inlinedWhenOnObject) 3 else 2, "built-in <clone>() operations") {
        case UnaryOp(UnaryOp.Clone, _) => true
      }.hasExactly(if (inlinedWhenOnObject) 0 else 1, "call to clone() (not inlined)") {
        case Apply(_, _, MethodIdent(`cloneMethodName`), _) => true
      }
    }
  }

  /** Test array `clone()` inlining when `j.l.Object.clone()` is not otherwise
   *  reachable.
   */
  @Test
  def testCloneOnArrayInliningNonObjectCloneOnly_Issue3778(): AsyncResult = await {
    testCloneOnArrayInliningGeneric(inlinedWhenOnObject = false,
        Vector(
          // @inline override def clone(): AnyRef = witness()
          MethodDef(EMF, cloneMethodName, NON, Vector(), AnyType, Some {
            Apply(EAF, thisFor("Foo"), witnessMethodName, Vector())(AnyType)
          })(EOH.withInline(true), UNV)
        ))
  }

  /** Test array `clone()` inlining when `j.l.Object.clone()` is the only
   *  reachable `clone()` method.
   *
   *  In that case, it will be inlined even when called on a receiver whose
   *  static type is no more precise than `Object`.
   */
  @Test
  def testCloneOnArrayInliningObjectCloneOnly_Issue3778(): AsyncResult = await {
    testCloneOnArrayInliningGeneric(inlinedWhenOnObject = true, Vector())
  }

  /** Test array `clone()` inlining when `j.l.Object.clone()` and another
   *  `clone()` method are reachable.
   */
  @Test
  def testCloneOnArrayInliningObjectCloneAndAnotherClone_Issue3778(): AsyncResult = await {
    testCloneOnArrayInliningGeneric(inlinedWhenOnObject = false,
        Vector(
          // @inline override def clone(): AnyRef = witness()
          MethodDef(EMF, cloneMethodName, NON, Vector(), AnyType, Some {
            Block(
              Apply(EAF, thisFor("Foo"), witnessMethodName, Vector())(AnyType),
              ApplyStatically(EAF, thisFor("Foo"),
                  ObjectClass, cloneMethodName, Vector())(AnyType)
            )
          })(EOH.withInline(true), UNV)
        ))
  }

  /** Makes sure that a hello world does not need java.lang.Class after
   *  optimizations.
   */
  @Test
  def testHelloWorldDoesNotNeedClassClass(): AsyncResult = await {
    val classDefs = Seq(
      mainTestClassDef {
        systemOutPrintln(str("Hello world!"))
      }
    )

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers,
          stdlib = TestIRRepo.javalib)
    } yield {
      assertFalse(findClass(moduleSet, ClassClass).isDefined)
    }
  }

  @Test
  def testOptimizerDoesNotEliminateRequiredStaticField_Issue4021(): AsyncResult = await {
    val StringType = ClassType(BoxedStringClass, nullable = true, exact = false)
    val fooGetter = m("foo", Vector(), T)
    val classDefs = Seq(
      classDef(
        MainTestClassName,
        kind = ClassKind.Class,
        superClass = Some(ObjectClass),
        fields = Vector(
          // static var foo: java.lang.String
          FieldDef(EMF.withNamespace(PublicStatic).withMutable(true),
              FieldName(MainTestClassName, "foo"), NON, StringType)
        ),
        methods = Vector(
          trivialCtor(MainTestClassName),
          // static def foo(): java.lang.String = Test+:foo
          MethodDef(EMF.withNamespace(MemberNamespace.PublicStatic),
              fooGetter, NON, Vector(), StringType, Some {
                SelectStatic(FieldName(MainTestClassName, "foo"))(StringType)
              })(EOH, UNV),
          // static def main(args: String[]) { println(Test+:foo()) }
          mainMethodDef {
            consoleLog(ApplyStatic(EAF, MainTestClassName, fooGetter, Vector())(StringType))
          }
        )
      )
    )

    for (moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)) yield {
      val mainClassDef = findClass(moduleSet, MainTestClassName).get
      assertTrue(mainClassDef.fields.exists {
        case FieldDef(_, FieldIdent(name), _, _) => name == FieldName(MainTestClassName, "foo")
        case _                                   => false
      })
    }
  }

  @Test
  def testOptimizerDoesNotEliminateRequiredLabeledBlockEmittedByDotty_Issue4171(): AsyncResult = {
    await {
      /* For the following source code:
       *
       * (null: Any) match {
       *   case (_: Int) | (_: String) =>
       * }
       *
       * the dotty compiler generates the following IR:
       *
       * matchResult1: {
       *   val x1: any = null;
       *   matchAlts1: {
       *     matchAlts2: {
       *       if (x1.isInstanceOf[java.lang.Integer!]) {
       *         return@matchAlts2 (void 0)
       *       };
       *       if (x1.isInstanceOf[java.lang.String!]) {
       *         return@matchAlts2 (void 0)
       *       };
       *       return@matchAlts1 (void 0)
       *     };
       *     return@matchResult1 (void 0)
       *   };
       *   throw new scala.MatchError().<init>;Ljava.lang.Object;V(x1)
       * }
       *
       * The optimizer used to erroneously get rid of the `matchAlts1` labeled
       * block, although it could not remove the `return@matchAlts1`. This led to
       * a crash in the Emitter.
       *
       * This test reproduces that exact IR by hand, and verifies that the
       * optimized code can be linked all the way to the Emitter.
       */

      val matchResult1 = LabelName("matchResult1")
      val x1 = LocalName("x1")
      val matchAlts1 = LabelName("matchAlts1")
      val matchAlts2 = LabelName("matchAlts2")

      val boxedIntegerType = ClassType(BoxedIntegerClass, nullable = false, exact = false)

      val results = for (voidReturnArgument <- Vector(Undefined(), Skip())) yield {
        val classDefs = Seq(
          mainTestClassDef(Block(
            Labeled(matchResult1, VoidType,
                Block(
                  VarDef(x1, NON, AnyType, mutable = false, Null()),
                  Labeled(matchAlts1, VoidType,
                      Block(
                        Labeled(matchAlts2, VoidType,
                            Block(
                              If(IsInstanceOf(VarRef(x1)(AnyType), boxedIntegerType), {
                                Return(voidReturnArgument, matchAlts2)
                              }, Skip())(VoidType),
                              If(IsInstanceOf(VarRef(x1)(AnyType), boxedIntegerType), {
                                Return(voidReturnArgument, matchAlts2)
                              }, Skip())(VoidType),
                              Return(voidReturnArgument, matchAlts1)
                            )),
                        Return(voidReturnArgument, matchResult1)
                      )),
                  UnaryOp(UnaryOp.Throw, New("java.lang.Exception", NoArgConstructorName, Vector()))
                ))
          ))
        )

        testLink(classDefs, MainTestModuleInitializers)
      }

      Future.sequence(results)
    }
  }

  @Test
  def testCaptureElimination(): AsyncResult = await {
    val sideEffect = m("sideEffect", Vector(I), I)

    val x = LocalName("x")
    val x2 = LocalName("x2")
    val x4 = LocalName("x4")

    val classDefs = Seq(
      classDef(
        MainTestClassName,
        kind = ClassKind.Class,
        superClass = Some(ObjectClass),
        methods = Vector(
          trivialCtor(MainTestClassName),
          // @noinline static def sideEffect(x: Int): Int = x
          MethodDef(EMF.withNamespace(PublicStatic), sideEffect, NON,
              Vector(paramDef(x, IntType)), IntType, Some(VarRef(x)(IntType)))(
              EOH.withNoinline(true), UNV),
          /* static def main(args: String[]) {
           *   console.log(arrow-lambda<
           *     x1 = sideEffect(1),
           *     x2 = sideEffect(2),
           *     ...
           *     x5 = sideEffect(5),
           *   >() = {
           *     console.log(x4);
           *     console.log(x2);
           *   });
           * }
           */
          mainMethodDef {
            val closure = Closure(
              ClosureFlags.arrow,
              (1 to 5).toVector.map(i => paramDef(LocalName("x" + i), IntType)),
              Vector(),
              None,
              AnyType,
              Block(
                consoleLog(VarRef(x4)(IntType)),
                consoleLog(VarRef(x2)(IntType))
              ),
              (1 to 5).toVector.map(
                  i => ApplyStatic(EAF, MainTestClassName, sideEffect, Vector(int(i)))(IntType))
            )
            consoleLog(closure)
          }
        )
      )
    )

    for (moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)) yield {
      var lastSideEffectFound = 0
      var closureParams = Vector.empty[Vector[LocalName]]
      traverseMainMethod(moduleSet) {
        case ApplyStatic(_, MainTestClassName, MethodIdent(`sideEffect`), Vector(IntLiteral(i))) =>
          assertEquals("wrong side effect ordering", lastSideEffectFound + 1, i)
          lastSideEffectFound = i
        case c: Closure =>
          closureParams :+= c.captureParams.map(_.name.name)
        case _ =>
      }
      assertEquals("wrong number of side effect calls", 5, lastSideEffectFound)
      assertEquals("wrong closure params", Vector(Vector(x2, x4)), closureParams)
    }
  }

  @Test
  def testOptimizeDynamicImport(): AsyncResult = await {
    val thunkMethodName = m("thunk", Vector(), O)
    val implMethodName = m("impl", Vector(), O)

    val memberMethodName = m("member", Vector(), O)

    val SMF = EMF.withNamespace(MemberNamespace.PublicStatic)

    val classDefs = Seq(
      mainTestClassDef {
        consoleLog(ApplyDynamicImport(EAF, "Thunk", thunkMethodName, Vector()))
      },
      classDef(
        "Thunk",
        superClass = Some(ObjectClass),
        optimizerHints = EOH.withInline(true),
        methods = Vector(
          trivialCtor("Thunk"),
          MethodDef(EMF, implMethodName, NON, Vector(), AnyType, Some {
            SelectJSNativeMember("Holder", memberMethodName)
          })(EOH, UNV),
          MethodDef(SMF, thunkMethodName, NON, Vector(), AnyType, Some {
            val inst = New("Thunk", NoArgConstructorName, Vector())
            Apply(EAF, inst, implMethodName, Vector())(AnyType)
          })(EOH, UNV)
        )
      ),
      classDef(
        "Holder",
        kind = ClassKind.Interface,
        jsNativeMembers = Vector(
          JSNativeMemberDef(SMF, memberMethodName, JSNativeLoadSpec.Import("foo", Vector("bar")))
        )
      )
    )

    val linkerConfig = StandardConfig()
      .withModuleKind(ModuleKind.ESModule)

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers,
          config = linkerConfig)
    } yield {
      assertFalse(findClass(moduleSet, "Thunk").isDefined)

      var foundJSImport = false
      val main = findClass(moduleSet, MainTestClassName).get
      val traverser = new Traverser {
        override def traverse(tree: Tree): Unit = tree match {
          case tree: ApplyDynamicImport => fail(s"found ApplyDynamicImport " + tree)
          case tree: JSImportCall       => foundJSImport = true
          case tree                     => super.traverse(tree)
        }
      }

      main.methods.foreach(traverser.traverseMethodDef(_))

      assertTrue(foundJSImport)
    }
  }

  @Test
  def testFoldLiteralClosureCaptures(): AsyncResult = await {
    val classDefs = Seq(
      mainTestClassDef {
        consoleLog(Closure(ClosureFlags.arrow, Vector(paramDef("x", IntType)), Vector(), None, AnyType, {
          BinaryOp(BinaryOp.Int_+, VarRef("x")(IntType), int(2))
        }, Vector(int(3))))
      }
    )

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)
    } yield {
      traverseMainMethod(moduleSet) {
        case c: Closure =>
          assertTrue(s"captures remaining: $c", c.captureParams.isEmpty)
          assertTrue(s"body is not a constant: $c", c.body.isInstanceOf[IntLiteral])
        case _ =>
      }
    }
  }

  @Test
  def testSupportImplicitClosureCaptures(): AsyncResult = await {
    val calc = m("calc", Vector(), I)

    val classDefs = Seq(
      classDef(
        MainTestClassName,
        kind = ClassKind.Class,
        superClass = Some(ObjectClass),
        methods = Vector(
          // @noinline static def calc(): Int = 1
          MethodDef(EMF.withNamespace(PublicStatic), calc, NON, Vector(),
              IntType, Some(int(1)))(EOH.withNoinline(true), UNV),
          mainMethodDef(Block(
            VarDef("x", NON, IntType, mutable = false,
                ApplyStatic(EAF, MainTestClassName, calc, Vector())(IntType)),
            consoleLog(Closure(ClosureFlags.arrow, Vector(paramDef("y", IntType)), Vector(), None,
                AnyType, VarRef("y")(IntType), Vector(VarRef("x")(IntType))))
          ))
        )
      )
    )

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)
    } yield {
      traverseMainMethod(moduleSet) {
        case c: Closure =>
          c.captureValues match {
            case Vector(VarRef(name)) =>
              assertEquals(s"unexpected capture name: $c", c.captureParams.head.name.name, name)

            case _ =>
              fail(s"unexpected capture values: $c")
          }

        case _ =>
      }
    }
  }

  private def commonClassDefsForFieldRemovalTests(classInline: Boolean,
      witnessMutable: Boolean): Seq[ClassDef] = {
    val methodName = m("method", Vector(), I)

    val witnessType = ClassType("Witness", nullable = true, exact = false)

    Seq(
      classDef("Witness", kind = ClassKind.Interface),
      classDef(
        "Foo",
        kind = ClassKind.Class,
        superClass = Some(ObjectClass),
        fields = Vector(
          // x: Witness
          FieldDef(EMF.withMutable(witnessMutable), FieldName("Foo", "x"), NON, witnessType),

          // y: Int
          FieldDef(EMF, FieldName("Foo", "y"), NON, IntType)
        ),
        methods = Vector(
          // def this() = {
          //   this.x = null
          //   this.y = 5
          //   this.jl.Object+:<init>()
          // }
          MethodDef(EMF.withNamespace(Constructor), NoArgConstructorName, NON, Vector(), VoidType,
              Some(Block(
                Assign(Select(thisFor("Foo"), FieldName("Foo", "x"))(witnessType), Null()),
                Assign(Select(thisFor("Foo"), FieldName("Foo", "y"))(IntType), int(5)),
                trivialSuperCtorCall("Foo")
              )))(EOH, UNV),

          // def method(): Int = this.y
          MethodDef(EMF, methodName, NON, Vector(), IntType, Some {
            Select(thisFor("Foo"), FieldName("Foo", "y"))(IntType)
          })(EOH, UNV)
        ),
        optimizerHints = EOH.withInline(classInline)
      ),
      mainTestClassDef {
        consoleLog(Apply(EAF, New("Foo", NoArgConstructorName, Vector()), methodName, Vector())(IntType))
      }
    )
  }

  @Test
  def removeUnusedFields(): AsyncResult = await {
    val classDefs = commonClassDefsForFieldRemovalTests(classInline = false, witnessMutable = false)

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)
    } yield {
      findClass(moduleSet, "Foo").get.fields match {
        case Vector(FieldDef(_, FieldIdent(name), _, _)) if name == FieldName("Foo", "y") =>
          // ok

        case fields =>
          fail(s"Unexpected fields: $fields")
      }

      assertFalse(findClass(moduleSet, "Witness").isDefined)
    }
  }

  @Test
  def removeUnusedFieldsInline(): AsyncResult = await {
    val classDefs = commonClassDefsForFieldRemovalTests(classInline = true, witnessMutable = false)

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)
    } yield {
      assertFalse(findClass(moduleSet, "Foo").isDefined)
      assertFalse(findClass(moduleSet, "Witness").isDefined)
    }
  }

  @Test
  def removeUnusedMutableFieldsInline(): AsyncResult = await {
    val classDefs = commonClassDefsForFieldRemovalTests(classInline = true, witnessMutable = true)

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)
    } yield {
      assertFalse(findClass(moduleSet, "Foo").isDefined)
      assertFalse(findClass(moduleSet, "Witness").isDefined)
    }
  }

  def inlineFlagsTestCommon(optimizerHints: OptimizerHints, applyFlags: ApplyFlags,
      expectInline: Boolean): AsyncResult = await {
    val classDefs = Seq(
      classDef(
        MainTestClassName,
        kind = ClassKind.Class,
        superClass = Some(ObjectClass),
        methods = Vector(
          trivialCtor(MainTestClassName),
          MethodDef(EMF.withNamespace(PublicStatic), witnessMethodName, NON, Vector(), AnyType, Some {
            // Non-trivial body to ensure no inlining by heuristics.
            Block(consoleLog(str("something")), str("result"))
          })(optimizerHints, UNV),
          mainMethodDef {
            consoleLog {
              ApplyStatic(applyFlags, MainTestClassName, witnessMethodName, Vector())(AnyType)
            }
          }
        )
      )
    )

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)
    } yield {
      val didInline = !findClass(moduleSet, MainTestClassName).get
        .methods.exists(_.name.name == witnessMethodName)

      assertEquals(expectInline, didInline)
    }
  }

  @Test
  def inlineFlagsTestDefault(): AsyncResult =
    inlineFlagsTestCommon(EOH, EAF, expectInline = false)

  @Test
  def inlineFlagsTestInlineDefSite(): AsyncResult =
    inlineFlagsTestCommon(EOH.withInline(true), EAF, expectInline = true)

  @Test
  def inlineFlagsTestNoinlineDefSite(): AsyncResult =
    inlineFlagsTestCommon(EOH.withNoinline(true), EAF, expectInline = false)

  @Test
  def inlineFlagsTestInlineCallSite(): AsyncResult =
    inlineFlagsTestCommon(EOH, EAF.withInline(true), expectInline = true)

  @Test
  def inlineFlagsTestNoinlineDefSiteNoOverride(): AsyncResult =
    inlineFlagsTestCommon(EOH.withNoinline(true), EAF.withInline(true), expectInline = false)

  @Test
  def inlineFlagsTestNoinlineCallSite(): AsyncResult =
    inlineFlagsTestCommon(EOH, EAF.withNoinline(true), expectInline = false)

  @Test
  def inlineFlagsTestNoinlineCallSiteOverride(): AsyncResult =
    inlineFlagsTestCommon(EOH.withInline(true), EAF.withNoinline(true), expectInline = false)
}

object OptimizerTest {
  private val cloneMethodName = m("clone", Vector(), O)
  private val witnessMethodName = m("witness", Vector(), O)

  private def findClass(moduleSet: ModuleSet, name: ClassName): Option[LinkedClass] =
    moduleSet.modules.flatMap(_.classDefs).find(_.className == name)

  private def traverseMainMethod(moduleSet: ModuleSet)(f: Tree => Unit) = {
    val mainClassDef = findClass(moduleSet, MainTestClassName).get
    val mainMethodDef = mainClassDef.methods.find(
        m => m.name.name == MainMethodName && m.flags.namespace == MemberNamespace.PublicStatic).get

    new Traverser {
      override def traverse(tree: Tree): Unit = {
        f(tree)
        super.traverse(tree)
      }
    }.traverseMethodDef(mainMethodDef)
  }
}
