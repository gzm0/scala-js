package build

/*
 * Hard-coded IR for java.lang.Object.
 */

import java.io.ByteArrayOutputStream

import org.scalajs.ir
import org.scalajs.ir._
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position.NoPosition
import org.scalajs.ir.Version.Unversioned
import org.scalajs.ir.WellKnownNames._

/** Hard-coded IR for java.lang.Object.
 *  We cannot so much as begin to fake a compilation of java.lang.Object,
 *  because Object is hijacked so much by scalac itself that it does not like
 *  at all to try to compile that class. So we have to bypass entirely the
 *  compiler to define java.lang.Object.
 */
object JavaLangObject {
  private val TheClassDef = {
    implicit val DummyPos = NoPosition

    // ClassType(Object) is normally invalid, but not in this class def
    val ThisType = ClassType(ObjectClass, nullable = false, exact = false)

    val ObjectClassRef = ClassRef(ObjectClass)
    val ClassClassRef = ClassRef(ClassClass)
    val StringClassRef = ClassRef(BoxedStringClass)

    val EAF = ApplyFlags.empty

    val classDef = ClassDef(
      ClassIdent(ObjectClass),
      NoOriginalName,
      ClassKind.Class,
      None,
      None,
      Vector(),
      None,
      None,
      fields = Vector(),
      Vector(
        /* def this() = () */
        MethodDef(
          MemberFlags.empty.withNamespace(MemberNamespace.Constructor),
          MethodIdent(NoArgConstructorName),
          NoOriginalName,
          Vector(),
          VoidType,
          Some(Skip()))(OptimizerHints.empty, Unversioned),

        /* def getClass(): java.lang.Class[_] = <getclass>(this) */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("getClass", Vector(), ClassClassRef)),
          NoOriginalName,
          Vector(),
          ClassType(ClassClass, nullable = true, exact = false),
          Some {
            UnaryOp(UnaryOp.GetClass, This()(ThisType))
          })(OptimizerHints.empty.withInline(true), Unversioned),

        /* def hashCode(): Int = <identityHashCode>(this) */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("hashCode", Vector(), IntRef)),
          NoOriginalName,
          Vector(),
          IntType,
          Some {
            UnaryOp(UnaryOp.IdentityHashCode, This()(ThisType))
          })(OptimizerHints.empty.withInline(true), Unversioned),

        /* def equals(that: Object): Boolean = this eq that */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("equals", Vector(ObjectClassRef), BooleanRef)),
          NoOriginalName,
          Vector(ParamDef(LocalIdent(LocalName("that")), NoOriginalName, AnyType,
            mutable = false)),
          BooleanType,
          Some {
            BinaryOp(BinaryOp.===,
              This()(ThisType),
              VarRef(LocalName("that"))(AnyType))
          })(OptimizerHints.empty.withInline(true), Unversioned),

        /* protected def clone(): Object =
         *   if (this.isInstanceOf[Cloneable]) <clone>(this.asInstanceOf[Cloneable])
         *   else throw new CloneNotSupportedException()
         */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("clone", Vector(), ObjectClassRef)),
          NoOriginalName,
          Vector(),
          AnyType,
          Some {
            If(IsInstanceOf(This()(ThisType), ClassType(CloneableClass, nullable = false, exact = false)), {
              UnaryOp(UnaryOp.Clone, UnaryOp(UnaryOp.CheckNotNull,
                  AsInstanceOf(This()(ThisType), ClassType(CloneableClass, nullable = true, exact = false))))
            }, {
              UnaryOp(UnaryOp.Throw, New(ClassName("java.lang.CloneNotSupportedException"),
                MethodIdent(NoArgConstructorName), Vector()))
            })(AnyType)
          })(OptimizerHints.empty.withInline(true), Unversioned),

        /* def toString(): String =
         *   getClass().getName() + "@" + Integer.toHexString(hashCode())
         */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("toString", Vector(), StringClassRef)),
          NoOriginalName,
          Vector(),
          ClassType(BoxedStringClass, nullable = true, exact = false),
          Some {
            BinaryOp(BinaryOp.String_+, BinaryOp(BinaryOp.String_+,
              Apply(
                EAF,
                Apply(EAF, This()(ThisType),
                  MethodIdent(MethodName("getClass", Vector(), ClassClassRef)), Vector())(
                  ClassType(ClassClass, nullable = true, exact = false)),
                MethodIdent(MethodName("getName", Vector(), StringClassRef)), Vector())(
                ClassType(BoxedStringClass, nullable = true, exact = false)),
              // +
              StringLiteral("@")),
              // +
              Apply(
                EAF,
                LoadModule(ClassName("java.lang.Integer$")),
                MethodIdent(MethodName("toHexString", Vector(IntRef), StringClassRef)),
                Vector(Apply(EAF, This()(ThisType), MethodIdent(MethodName("hashCode", Vector(), IntRef)), Vector())(IntType)))(
                ClassType(BoxedStringClass, nullable = true, exact = false)))
          })(OptimizerHints.empty, Unversioned),

        /* Since wait() is not supported in any way, a correct implementation
         * of notify() and notifyAll() is to do nothing.
         */

        /* def notify(): Unit = () */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("notify", Vector(), VoidRef)),
          NoOriginalName,
          Vector(),
          VoidType,
          Some(Skip()))(OptimizerHints.empty, Unversioned),

        /* def notifyAll(): Unit = () */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("notifyAll", Vector(), VoidRef)),
          NoOriginalName,
          Vector(),
          VoidType,
          Some(Skip()))(OptimizerHints.empty, Unversioned),

        /* def finalize(): Unit = () */
        MethodDef(
          MemberFlags.empty,
          MethodIdent(MethodName("finalize", Vector(), VoidRef)),
          NoOriginalName,
          Vector(),
          VoidType,
          Some(Skip()))(OptimizerHints.empty, Unversioned),
      ),
      jsConstructor = None,
      jsMethodProps = Vector(
        /* JSExport for toString(). */
        JSMethodDef(
          MemberFlags.empty,
          StringLiteral("toString"),
          Vector(), None,
          {
            Apply(EAF, This()(ThisType),
                MethodIdent(MethodName("toString", Vector(), StringClassRef)), Vector())(
                ClassType(BoxedStringClass, nullable = true, exact = false))
          })(OptimizerHints.empty, Unversioned)
      ),
      jsNativeMembers = Vector(),
      topLevelExportDefs = Vector())(OptimizerHints.empty)

    Hashers.hashClassDef(classDef)
  }

  val irBytes: Array[Byte] = {
    val stream = new ByteArrayOutputStream
    try ir.Serializers.serialize(stream, TheClassDef)
    finally stream.close()
    stream.toByteArray
  }
}
