package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import org.objectweb.asm.Label
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

object GenChannelClasses {
  /**
    * Returns the bytecode for the cell classes built-in to the Flix language.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {

    // Type that we need a cell class for
    val types = List(JvmType.PrimBool, JvmType.PrimChar, JvmType.PrimFloat, JvmType.PrimDouble,
      JvmType.PrimByte, JvmType.PrimShort, JvmType.PrimInt, JvmType.Tuple, JvmType.Unit,
      JvmType.PrimLong, JvmType.Channel, JvmType.Object)

    // Generating each channel class
    types.map{ tpe =>
      val classType = JvmName.getChannelClassType(tpe)
      classType.name -> JvmClass(classType.name, genChannelClass(classType, tpe))
    }.toMap
  }

  def genChannelClass(classType: JvmType.Reference, channelType: JvmType)(implicit root: Root, flix: Flix): Array[Byte] = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null,
      JvmName.Object.toInternalName, null)

    // Generate the instance field
    AsmOps.compileField(visitor, "queue", JvmType.BlockingQueue, isStatic = false, isPrivate = true)

    // Generate the constructor
    genConstructor(classType, channelType, visitor)

    // Generate `getValue` method
    genGetValue(classType, channelType, visitor)

    // Generate `putValue` method
    //genPutValue(classType, channelType, visitor)

    // Generate the `toString` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String),
      "toString method shouldn't be called")

    // Generate the `hashCode` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "hashCode", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt),
      "hashCode method shouldn't be called")

    // Generate the `equals` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool),
      "equals method shouldn't be called")

    // Complete the visitor and get the bytecode.
    visitor.visitEnd()
    visitor.toByteArray
  }

  def genConstructor(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val initMethod = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(List(JvmType.PrimInt), JvmType.Void), null, null)
    val condElse = new Label()
    val condEnd = new Label()
    initMethod.visitCode()
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitInsn(DUP)
    initMethod.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitTypeInsn(NEW, "java/util/LinkedList")
    initMethod.visitInsn(DUP)
    initMethod.visitMethodInsn(INVOKESPECIAL, "java/util/LinkedList", "<init>", "()V", false)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "queue", JvmType.BlockingQueue.toDescriptor)
    initMethod.visitJumpInsn(GOTO, condEnd)
    initMethod.visitLabel(condElse)
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitTypeInsn(NEW, "java/util/concurrent/LinkedBlockingQueue")
    initMethod.visitInsn(DUP)
    initMethod.visitVarInsn(ILOAD, 1)
    initMethod.visitMethodInsn(INVOKESPECIAL, "java/util/concurrent/LinkedBlockingQueue", "<init>", "(I)V", false)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "queue", JvmType.BlockingQueue.toDescriptor)
    initMethod.visitLabel(condEnd)
    initMethod.visitInsn(RETURN)
    initMethod.visitMaxs(2, 2)
    initMethod.visitEnd()
  }

  def genGetValue(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val iLoad = AsmOps.getLoadInstruction(channelType)
    val iRet = AsmOps.getReturnInstruction(channelType)
    val getValue = visitor.visitMethod(ACC_PUBLIC, "getValue", AsmOps.getMethodDescriptor(Nil, channelType), null, null)
    getValue.visitCode()

    getValue.visitVarInsn(ALOAD, 0)
    getValue.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.BlockingQueue.toDescriptor)
    getValue.visitMethodInsn(INVOKEINTERFACE, "java/util/concurrent/BlockingQueue", "take", AsmOps.getMethodDescriptor(Nil, channelType), true)
    getValue.visitInsn(iRet)
    getValue.visitMaxs(1, 1)
    getValue.visitEnd()
  }

  def genPutValue(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val putValue = visitor.visitMethod(ACC_PUBLIC, "putValue", AsmOps.getMethodDescriptor(List(channelType), classType), null, null)
    putValue.visitCode()

    putValue.visitVarInsn(ALOAD, 0)
    putValue.visitInsn(ARETURN)
    putValue.visitEnd()
  }
}
