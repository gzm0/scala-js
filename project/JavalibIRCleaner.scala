package build

import org.scalajs.ir._
import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import java.io._
import java.net.URI
import java.nio.file.Files

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable

import sbt.{Logger, MessageOnlyException}

/** Postprocessor for the IR of the javalanglib, to remove all references to
 *  JS types in the Scala library, and ensure that there remains no other
 *  reference to the Scala library.
 *
 *  This ensures that the IR of the javalanglib is truly independent of Scala.
 *
 *  The main task is to *completely* erase all references to JS types to
 *  `j.l.Object`. This includes:
 *
 *  - Delete (or do not copy over) .sjsir files that define abstract and native
 *    JS types.
 *  - Erase references to JS types in method signatures and `TypeRef`s to
 *    `java.lang.Object`.
 *  - Eagerly dereference `LoadJSConstructor` and `LoadJSModule` by "inlining"
 *    the JS load spec of the mentioned class ref.
 *  - Replace calls to "intrinsic" methods of the Scala.js library by their
 *    meaning at call site.
 *  - Erase Scala FunctionN's to JavaScript closures (hence, of type `any`),
 *    including `new AnonFunctionN` and `someFunctionN.apply` calls.
 *
 *  Afterwards, we check that the IR does not contain any reference to classes
 *  under the `scala.*` package.
 */
final class JavalibIRCleaner(baseDirectoryURI: URI) {
  import JavalibIRCleaner._

  def cleanIR(dependencyFiles: Seq[File], libFileMappings: Seq[(File, File)],
      logger: Logger): Set[File] = {

    val errorManager = new ErrorManager(logger)

    val libIRMappings = for {
      (input, output) <- libFileMappings
    } yield {
      (readIR(input), output)
    }

    val jsTypes = {
      val dependencyIR = dependencyFiles.iterator.map(readIR(_))
      val libIR = libIRMappings.iterator.map(_._1)
      getJSTypes(dependencyIR ++ libIR)
    }

    val resultBuilder = Set.newBuilder[File]

    for ((tree, output) <- libIRMappings) {
      import ClassKind._

      tree.kind match {
        case Class | ModuleClass | Interface | HijackedClass =>
          val cleanedTree = cleanTree(tree, jsTypes, errorManager)
          writeIRFile(output, cleanedTree)
          resultBuilder += output

        case AbstractJSType | NativeJSClass | NativeJSModuleClass =>
          // discard

        case JSClass | JSModuleClass =>
          errorManager.reportError(
              s"found non-native JS class ${tree.className.nameString}")(tree.pos)
      }
    }

    if (errorManager.hasErrors) {
      throw new MessageOnlyException(
          s"There were ${errorManager.errorCount} errors while " +
          "postprocessing the IR of the javalanglib. " +
          "The javalanglib must be written in a style that does not leak any " +
          "reference to the Scala library.")
    }

    resultBuilder.result()
  }

  private final class ErrorManager(logger: Logger) {
    private val seenErrors = mutable.Set.empty[String]
    private var _errorCount: Int = 0

    def reportError(msg: String)(implicit pos: Position): Unit = {
      val fileStr = baseDirectoryURI.relativize(pos.source).toString
      val fullMessage = s"$msg at $fileStr:${pos.line}:${pos.column}"
      if (seenErrors.add(fullMessage)) {
        logger.error(fullMessage)
        _errorCount += 1
      }
    }

    def hasErrors: Boolean = _errorCount != 0

    def errorCount: Int = _errorCount
  }

  private def readIR(file: File): ClassDef = {
    import java.nio.ByteBuffer

    val bytes = Files.readAllBytes(file.toPath())
    val buffer = ByteBuffer.wrap(bytes)
    Serializers.deserialize(buffer)
  }

  private def writeIRFile(file: File, tree: ClassDef): Unit = {
    Files.createDirectories(file.toPath().getParent())
    val outputStream =
      new BufferedOutputStream(new FileOutputStream(file))
    try {
      Serializers.serialize(outputStream, tree)
    } finally {
      outputStream.close()
    }
  }

  private def getJSTypes(trees: Iterator[ClassDef]): Map[ClassName, ClassDef] =
    trees.filter(_.kind.isJSType).map(t => t.className -> t).toMap

  private def cleanTree(tree: ClassDef, jsTypes: Map[ClassName, ClassDef],
      errorManager: ErrorManager): ClassDef = {
    new ClassDefCleaner(tree.className, jsTypes, errorManager)
      .cleanClassDef(tree)
  }

  private final class ClassDefCleaner(enclosingClassName: ClassName,
      jsTypes: Map[ClassName, ClassDef], errorManager: ErrorManager)
      extends Transformers.ClassTransformer {

    def cleanClassDef(tree: ClassDef): ClassDef = {
      import tree._

      // Preprocess the super interface list
      val newInterfaces = transformInterfaceList(interfaces)

      /* Remove the `private def writeReplace__O` generated by scalac 2.13+
       * in the companion of serializable classes.
       */
      val newMemberDefs = memberDefs.filter {
        case MethodDef(_, MethodIdent(`writeReplaceMethodName`), _, _, _, _) =>
          false
        case _ =>
          true
      }

      val preprocessedTree = ClassDef(name, originalName, kind, jsClassCaptures,
          superClass, newInterfaces, jsSuperClass, jsNativeLoadSpec,
          newMemberDefs, topLevelExportDefs)(
          optimizerHints)(pos)

      // Only validate the hierarchy; do not transform
      validateClassName(preprocessedTree.name.name)
      for (superClass <- preprocessedTree.superClass)
        validateClassName(superClass.name)
      for (interface <- preprocessedTree.interfaces)
        validateClassName(interface.name)

      val transformedClassDef =
        Hashers.hashClassDef(this.transformClassDef(preprocessedTree))

      postTransformChecks(transformedClassDef)
      transformedClassDef
    }

    private def transformInterfaceList(
        interfaces: List[ClassIdent]): List[ClassIdent] = {

      /* Replace references to scala.Serializable by java.io.Serializable.
       * This works around the fact that scalac adds scala.Serializable to the
       * companion object of any class that extends java.io.Serializable.
       */

      if (!interfaces.exists(_.name == ScalaSerializable)) {
        interfaces
      } else if (interfaces.exists(_.name == JavaIOSerializable)) {
        interfaces.filter(_.name != ScalaSerializable)
      } else {
        interfaces.map { ident =>
          if (ident.name == ScalaSerializable)
            ClassIdent(JavaIOSerializable)(ident.pos)
          else
            ident
        }
      }
    }

    override def transformMemberDef(memberDef: MemberDef): MemberDef = {
      super.transformMemberDef(memberDef) match {
        case m @ FieldDef(flags, name, originalName, ftpe) =>
          implicit val pos = m.pos
          FieldDef(flags, name, originalName, transformType(ftpe))
        case m @ MethodDef(flags, name, originalName, args, resultType, body) =>
          implicit val pos = m.pos
          MethodDef(flags, transformMethodIdent(name), originalName, transformParamDefs(args),
              transformType(resultType), body)(m.optimizerHints, m.hash)
        case m =>
          m
      }
    }

    private def transformParamDefs(paramDefs: List[ParamDef]): List[ParamDef] = {
      for (paramDef <- paramDefs) yield {
        implicit val pos = paramDef.pos
        val ParamDef(name, originalName, ptpe, mutable) = paramDef
        ParamDef(name, originalName, transformType(ptpe), mutable)
      }
    }

    override def transform(tree: Tree, isStat: Boolean): Tree = {
      implicit val pos = tree.pos

      val tree1 = preTransform(tree, isStat)
      val tree2 = if (tree1 eq tree) super.transform(tree, isStat) else transform(tree1, isStat)
      val result = postTransform(tree2, isStat)

      if (transformType(result.tpe) != result.tpe)
        reportError(s"the result type of a ${result.getClass().getSimpleName()} was not transformed")

      result
    }

    private def preTransform(tree: Tree, isStat: Boolean): Tree = {
      implicit val pos = tree.pos

      tree match {
        // new AnonFunctionN(closure)  -->  closure
        case New(AnonFunctionNClass(n), _, List(closure)) =>
          closure

        // someFunctionN.apply(args)  -->  someFunctionN(args)
        case Apply(ApplyFlags.empty, fun, MethodIdent(FunctionApplyMethodName(n)), args)
            if isFunctionNType(n, fun.tpe) =>
          JSFunctionApply(fun, args)

        case IntrinsicCall(JSAnyMod, `jsAnyFromIntMethodName`, List(arg)) =>
          arg
        case IntrinsicCall(JSAnyMod, `jsAnyFromStringMethodName`, List(arg)) =>
          arg
        case IntrinsicCall(JSDynamicImplicitsMod, `number2dynamicMethodName`, List(arg)) =>
          arg
        case IntrinsicCall(JSNumberOpsMod, `enableJSNumberOpsDoubleMethodName`, List(arg)) =>
          arg
        case IntrinsicCall(JSNumberOpsMod, `enableJSNumberOpsIntMethodName`, List(arg)) =>
          arg
        case IntrinsicCall(JSStringOpsMod, `enableJSStringOpsMethodName`, List(arg)) =>
          arg

        case IntrinsicCall(JSDynamicImplicitsMod, `truthValueMethodName`, List(arg)) =>
          AsInstanceOf(
              JSUnaryOp(JSUnaryOp.!, JSUnaryOp(JSUnaryOp.!, arg)),
              BooleanType)

        // 2.11 s"..." interpolator
        case Apply(
            ApplyFlags.empty,
            New(StringContextClass, MethodIdent(`stringContextCtorMethodName`),
                List(ScalaVarArgsReadOnlyLiteral(stringElems))),
            MethodIdent(`sMethodName`),
            List(ScalaVarArgsReadOnlyLiteral(valueElems))) =>
          if (stringElems.size != valueElems.size + 1) {
            reportError("Found s\"...\" interpolator but the sizes do not match")
            tree
          } else {
            val processedEscapesStringElems = stringElems.map { s =>
              (s: @unchecked) match {
                case StringLiteral(value) =>
                  StringLiteral(StringContext.processEscapes(value))
              }
            }
            val stringsIter = processedEscapesStringElems.iterator
            val valuesIter = valueElems.iterator
            var result: Tree = stringsIter.next()
            while (valuesIter.hasNext) {
              result = BinaryOp(BinaryOp.String_+, result, valuesIter.next())
              result = BinaryOp(BinaryOp.String_+, result, stringsIter.next())
            }
            result
          }

        case _ =>
          tree
      }
    }

    private object IntrinsicCall {
      def unapply(tree: Apply): Option[(ClassName, MethodName, List[Tree])] = tree match {
        case Apply(ApplyFlags.empty, LoadModule(moduleClassName), MethodIdent(methodName), args) =>
          Some(moduleClassName, methodName, args)
        case _ =>
          None
      }
    }

    private object ScalaVarArgsReadOnlyLiteral {
      def unapply(tree: Apply): Option[List[Tree]] = tree match {
        case IntrinsicCall(ScalaJSRuntimeMod, `toScalaVarArgsReadOnlyMethodName`,
            List(JSArrayConstr(args))) =>
          if (args.forall(_.isInstanceOf[Tree]))
            Some(args.map(_.asInstanceOf[Tree]))
          else
            None
        case _ =>
          None
      }
    }

    private def postTransform(tree: Tree, isStat: Boolean): Tree = {
      implicit val pos = tree.pos

      tree match {
        case VarDef(name, originalName, vtpe, mutable, rhs) =>
          VarDef(name, originalName, transformType(vtpe), mutable, rhs)

        case Labeled(label, tpe, body) =>
          Labeled(label, transformType(tpe), body)
        case If(cond, thenp, elsep) =>
          If(cond, thenp, elsep)(transformType(tree.tpe))
        case TryCatch(block, errVar, errVarOriginalName, handler) =>
          TryCatch(block, errVar, errVarOriginalName, handler)(transformType(tree.tpe))
        case Match(selector, cases, default) =>
          Match(selector, cases, default)(transformType(tree.tpe))

        case New(className, ctor, args) =>
          New(transformNonJSClassName(className), transformMethodIdent(ctor), args)
        case Select(qualifier, className, field) =>
          Select(qualifier, transformNonJSClassName(className), field)(transformType(tree.tpe))

        case t: Apply =>
          Apply(t.flags, t.receiver, transformMethodIdent(t.method), t.args)(
              transformType(t.tpe))
        case t: ApplyStatically =>
          ApplyStatically(t.flags, t.receiver,
              transformNonJSClassName(t.className),
              transformMethodIdent(t.method), t.args)(transformType(t.tpe))
        case t: ApplyStatic =>
          ApplyStatic(t.flags, transformNonJSClassName(t.className),
              transformMethodIdent(t.method), t.args)(transformType(t.tpe))

        case NewArray(typeRef, lengths) =>
          NewArray(transformArrayTypeRef(typeRef), lengths)
        case ArrayValue(typeRef, elems) =>
          ArrayValue(transformArrayTypeRef(typeRef), elems)
        case ArraySelect(array, index) =>
          ArraySelect(array, index)(transformType(tree.tpe))

        case IsInstanceOf(expr, testType) =>
          IsInstanceOf(expr, transformType(testType))
        case AsInstanceOf(expr, tpe) =>
          AsInstanceOf(expr, transformType(tpe))

        case LoadJSConstructor(className) =>
          genLoadFromLoadSpecOf(className)
        case LoadJSModule(className) =>
          genLoadFromLoadSpecOf(className)

        case t: ClassOf =>
          if (transformTypeRef(t.typeRef) != t.typeRef)
            reportError(s"illegal ClassOf(${t.typeRef})")
          t

        case t @ VarRef(ident) =>
          VarRef(ident)(transformType(t.tpe))

        case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
          Closure(arrow, transformParamDefs(captureParams), transformParamDefs(params),
              restParam, body, captureValues)

        case _ =>
          tree
      }
    }

    private def genLoadFromLoadSpecOf(className: ClassName)(
        implicit pos: Position): Tree = {
      jsTypes.get(className) match {
        case Some(classDef) =>
          classDef.jsNativeLoadSpec match {
            case Some(loadSpec) =>
              genLoadFromLoadSpec(loadSpec)
            case None =>
              reportError(
                  s"${className.nameString} does not have a load spec " +
                  "(this shouldn't have happened at all; bug in the compiler?)")
              JSGlobalRef("Object")
          }
        case None =>
          reportError(s"${className.nameString} is not a JS type")
          JSGlobalRef("Object")
      }
    }

    private def genLoadFromLoadSpec(loadSpec: JSNativeLoadSpec)(
        implicit pos: Position): Tree = {
      loadSpec match {
        case JSNativeLoadSpec.Global(globalRef, Nil) =>
          JSGlobalRef(globalRef)
        case _ =>
          reportError(
              s"unsupported load spec $loadSpec; " +
              "only @JSGlobal without `.` is supported")
          JSGlobalRef("Object")
      }
    }

    private def transformMethodIdent(ident: MethodIdent): MethodIdent = {
      implicit val pos = ident.pos
      MethodIdent(transformMethodName(ident.name))
    }

    private def transformClassRef(cls: ClassRef)(
        implicit pos: Position): ClassRef = {
      if (jsTypes.contains(cls.className))
        ClassRef(ObjectClass)
      else
        ClassRef(transformClassName(cls.className))
    }

    private def transformArrayTypeRef(typeRef: ArrayTypeRef)(
        implicit pos: Position): ArrayTypeRef = {
      typeRef.base match {
        case _: PrimRef =>
          typeRef
        case ClassRef(baseClassName) =>
          if (jsTypes.contains(baseClassName))
            ArrayTypeRef(ClassRef(ObjectClass), typeRef.dimensions)
          else
            ArrayTypeRef(ClassRef(transformClassName(baseClassName)), typeRef.dimensions)
      }
    }

    private def transformTypeRef(typeRef: TypeRef)(
        implicit pos: Position): TypeRef = typeRef match {
      case typeRef: PrimRef      => typeRef
      case typeRef: ClassRef     => transformClassRef(typeRef)
      case typeRef: ArrayTypeRef => transformArrayTypeRef(typeRef)
    }

    private def postTransformChecks(classDef: ClassDef): Unit = {
      // Check that no two methods have been erased to the same name
      val seenMethodNames = mutable.Set.empty[(MemberNamespace, MethodName)]
      for (m <- classDef.memberDefs) {
        m match {
          case MethodDef(flags, name, _, _, _, _) =>
            if (!seenMethodNames.add((flags.namespace, name.name))) {
              reportError(
                  s"duplicate method name ${name.name.nameString} after erasure")(
                  m.pos)
            }
          case _ =>
        }
      }
    }

    private def transformType(tpe: Type)(implicit pos: Position): Type = {
      tpe match {
        case ClassType(ObjectClass) =>
          // In java.lang.Object iself, there are ClassType(ObjectClass) that must be preserved as is.
          tpe
        case ClassType(cls) =>
          transformClassName(cls) match {
            case ObjectClass => AnyType
            case newCls      => ClassType(newCls)
          }
        case ArrayType(arrayTypeRef) =>
          ArrayType(transformArrayTypeRef(arrayTypeRef))
        case _ =>
          tpe
      }
    }

    private def transformClassName(cls: ClassName)(implicit pos: Position): ClassName = {
      ClassNameSubstitutions.getOrElse(cls, {
        validateClassName(cls)
        cls
      })
    }

    private def validateClassName(cls: ClassName)(implicit pos: Position): Unit = {
      def isJavaScriptExceptionWithinItself =
        cls == JavaScriptExceptionClass && enclosingClassName == JavaScriptExceptionClass

      if (cls.nameString.startsWith("scala.") && !isJavaScriptExceptionWithinItself)
        reportError(s"Illegal reference to Scala class ${cls.nameString}")
    }

    private def transformNonJSClassName(cls: ClassName)(implicit pos: Position): ClassName = {
      if (jsTypes.contains(cls)) {
        reportError(s"Invalid reference to JS class ${cls.nameString}")
        cls
      } else {
        transformClassName(cls)
      }
    }

    private def transformMethodName(name: MethodName)(implicit pos: Position): MethodName = {
      MethodName(name.simpleName, name.paramTypeRefs.map(transformTypeRef),
          transformTypeRef(name.resultTypeRef), name.isReflectiveProxy)
    }

    private def reportError(msg: String)(implicit pos: Position): Unit = {
      errorManager.reportError(s"$msg in ${enclosingClassName.nameString}")
    }
  }
}

object JavalibIRCleaner {
  private final val MaxFunctionArity = 4

  // Within js.JavaScriptException, which is part of the linker private lib, we can refer to itself
  private val JavaScriptExceptionClass = ClassName("scala.scalajs.js.JavaScriptException")

  private val JavaIOSerializable = ClassName("java.io.Serializable")
  private val JSAny = ClassName("scala.scalajs.js.Any")
  private val JSAnyMod = ClassName("scala.scalajs.js.Any$")
  private val JSArray = ClassName("scala.scalajs.js.Array")
  private val JSDynamic = ClassName("scala.scalajs.js.Dynamic")
  private val JSDynamicImplicitsMod = ClassName("scala.scalajs.js.DynamicImplicits$")
  private val JSNumberOps = ClassName("scala.scalajs.js.JSNumberOps")
  private val JSNumberOpsMod = ClassName("scala.scalajs.js.JSNumberOps$")
  private val JSStringOps = ClassName("scala.scalajs.js.JSStringOps")
  private val JSStringOpsMod = ClassName("scala.scalajs.js.JSStringOps$")
  private val ReadOnlySeq = ClassName("scala.collection.Seq")
  private val ScalaSerializable = ClassName("scala.Serializable")
  private val ScalaJSRuntimeMod = ClassName("scala.scalajs.runtime.package$")
  private val StringContextClass = ClassName("scala.StringContext")

  private val FunctionNClasses: IndexedSeq[ClassName] =
    (0 to MaxFunctionArity).map(n => ClassName(s"scala.Function$n"))

  private val AnonFunctionNClasses: IndexedSeq[ClassName] =
    (0 to MaxFunctionArity).map(n => ClassName(s"scala.scalajs.runtime.AnonFunction$n"))

  private val enableJSNumberOpsDoubleMethodName =
    MethodName("enableJSNumberOps", List(DoubleRef), ClassRef(JSNumberOps))
  private val enableJSNumberOpsIntMethodName =
    MethodName("enableJSNumberOps", List(IntRef), ClassRef(JSNumberOps))
  private val enableJSStringOpsMethodName =
    MethodName("enableJSStringOps", List(ClassRef(BoxedStringClass)), ClassRef(JSStringOps))
  private val jsAnyFromIntMethodName =
    MethodName("fromInt", List(IntRef), ClassRef(JSAny))
  private val jsAnyFromStringMethodName =
    MethodName("fromString", List(ClassRef(BoxedStringClass)), ClassRef(JSAny))
  private val number2dynamicMethodName =
    MethodName("number2dynamic", List(DoubleRef), ClassRef(JSDynamic))
  private val sMethodName =
    MethodName("s", List(ClassRef(ReadOnlySeq)), ClassRef(BoxedStringClass))
  private val stringContextCtorMethodName =
    MethodName.constructor(List(ClassRef(ReadOnlySeq)))
  private val toScalaVarArgsReadOnlyMethodName =
    MethodName("toScalaVarArgs", List(ClassRef(JSArray)), ClassRef(ReadOnlySeq))
  private val truthValueMethodName =
    MethodName("truthValue", List(ClassRef(JSDynamic)), BooleanRef)
  private val writeReplaceMethodName =
    MethodName("writeReplace", Nil, ClassRef(ObjectClass))

  private val functionApplyMethodNames: IndexedSeq[MethodName] = {
    (0 to MaxFunctionArity).map { n =>
      MethodName("apply", (1 to n).toList.map(_ => ClassRef(ObjectClass)), ClassRef(ObjectClass))
    }
  }

  private object AnonFunctionNClass {
    private val AnonFunctionNClassToN: Map[ClassName, Int] =
      AnonFunctionNClasses.zipWithIndex.toMap

    def apply(n: Int): ClassName = AnonFunctionNClasses(n)

    def unapply(cls: ClassName): Option[Int] = AnonFunctionNClassToN.get(cls)
  }

  private object FunctionApplyMethodName {
    private val FunctionApplyMethodNameToN: Map[MethodName, Int] =
      functionApplyMethodNames.zipWithIndex.toMap

    def apply(n: Int): MethodName = functionApplyMethodNames(n)

    def unapply(name: MethodName): Option[Int] = FunctionApplyMethodNameToN.get(name)
  }

  private def isFunctionNType(n: Int, tpe: Type): Boolean = tpe match {
    case ClassType(cls) =>
      cls == FunctionNClasses(n) || cls == AnonFunctionNClasses(n)
    case _ =>
      false
  }

  private val ClassNameSubstitutions: Map[ClassName, ClassName] = {
    val functionTypePairs = for {
      funClass <- FunctionNClasses ++ AnonFunctionNClasses
    } yield {
      funClass -> ObjectClass
    }

    val refBaseNames =
      List("Boolean", "Char", "Byte", "Short", "Int", "Long", "Float", "Double", "Object")
    val refPairs = for {
      refBaseName <- refBaseNames
    } yield {
      val simpleName = refBaseName + "Ref"
      ClassName("scala.runtime." + simpleName) -> ClassName("java.util.internal." + simpleName)
    }

    val allPairs = functionTypePairs ++ refPairs
    allPairs.toMap
  }
}
