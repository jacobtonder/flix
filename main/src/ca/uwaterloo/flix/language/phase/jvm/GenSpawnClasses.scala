package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Type}
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the Spawn class
  */
object GenSpawnClasses {
  /**
    * Returns the bytecode for the Spawn class.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val functionType = JvmOps.getFunctionInterfaceType(Type.mkArrow(Type.Unit, Type.Unit))

    val cont = Nil;

    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Class header.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, JvmName.Spawn.toInternalName, null,
      JvmName.Object.toInternalName, Array(JvmName.Runnable.toInternalName))

    visitor.visitSource(JvmName.Spawn.name, null)

    // Context field
    AsmOps.compileField(visitor, "ctx", JvmType.Context, isStatic = false, isPrivate = true)

    // Instance field
    AsmOps.compileField(visitor, "fn", functionType, isStatic = false, isPrivate = true)

    // Generate the constructor
    genConstructor(visitor)

    genRun(visitor)

    Map(JvmName.Spawn -> JvmClass(JvmName.Spawn, visitor.toByteArray))
  }

  /**
    * Generating constructor for the class `Spawn`
    */
  def genConstructor(visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val functionType = JvmOps.getFunctionInterfaceType(Type.mkArrow(Type.Unit, Type.Unit))
    val initMethod = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(List(functionType), JvmType.Void), null, null)
    initMethod.visitCode()

    // Base constructor call
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    // Set context
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitTypeInsn(NEW, JvmName.Context.toInternalName)
    initMethod.visitInsn(DUP)
    initMethod.visitMethodInsn(INVOKESPECIAL, JvmName.Context.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    initMethod.visitFieldInsn(PUTFIELD, JvmName.Spawn.toInternalName, "ctx", JvmType.Context.toDescriptor)

    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitVarInsn(ALOAD, 1)
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitFieldInsn(GETFIELD, JvmName.Spawn.toInternalName, "ctx", JvmType.Context.toDescriptor)
    initMethod.visitMethodInsn(INVOKEINTERFACE, functionType.name.toInternalName, "copy",
      AsmOps.getMethodDescriptor(List(JvmType.Context), functionType), true)
    initMethod.visitFieldInsn(PUTFIELD, JvmName.Spawn.toInternalName, "fn", functionType.toDescriptor)

    initMethod.visitInsn(RETURN)
    initMethod.visitMaxs(2, 2)
    initMethod.visitEnd()
  }

  /**
    * Generating run method for the class `Spawn`
    */
  def genRun(visitor: ClassWriter)(implicit  root: Root, flix: Flix): Unit = {
    val functionType = JvmOps.getFunctionInterfaceType(Type.mkArrow(Type.Unit, Type.Unit))
    val runMethod = visitor.visitMethod(ACC_PUBLIC, "run", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    runMethod.visitCode()
    runMethod.visitVarInsn(ALOAD, 0)
    runMethod.visitFieldInsn(GETFIELD, JvmName.Spawn.toInternalName, "fn", functionType.toDescriptor)

    runMethod.visitVarInsn(ALOAD, 0)
    runMethod.visitFieldInsn(GETFIELD, JvmName.Spawn.toInternalName, "ctx", JvmType.Context.toDescriptor)
    runMethod.visitMethodInsn(INVOKEINTERFACE, functionType.name.toInternalName, "apply",
      AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), true)

    runMethod.visitInsn(RETURN)
    runMethod.visitMaxs(1, 2)
    runMethod.visitEnd()
  }
}
