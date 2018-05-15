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
      JvmType.PrimLong, JvmType.Object)

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
    AsmOps.compileField(visitor, "queue", JvmType.LinkedList, isStatic = false, isPrivate = true)

    // Generate the capacity field
    AsmOps.compileField(visitor, "capacity", JvmType.PrimInt, isStatic = false, isPrivate = true)

    // Generate the channelLock field
    AsmOps.compileField(visitor, "channelLock", JvmType.Lock, isStatic = false, isPrivate = true)

    // Generate the channelNotFull field
    AsmOps.compileField(visitor, "channelNotFull", JvmType.Condition, isStatic = false, isPrivate = true)

    // Generate the channelNotEmpty field
    AsmOps.compileField(visitor, "channelNotEmpty", JvmType.Condition, isStatic = false, isPrivate = true)

    // Generate the conditions field
    AsmOps.compileField(visitor, "conditions", JvmType.JavaList, isStatic = false, isPrivate = true)

    // Generate the constructor
    genConstructor(classType, channelType, visitor)

    // Generate `getValue` method
    //genGetValue(classType, channelType, visitor)

    // Generate `getValue` method
    //genGetChannel(classType, channelType, visitor)

    // Generate `isEmpty`method
    genIsEmpty(classType, channelType, visitor)

    // Generate `size`method
    genSize(classType, channelType, visitor)

    // Generate `isFull`method
    genIsFull(classType, channelType, visitor)

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
    initMethod.visitCode()
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitInsn(DUP)
    initMethod.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitTypeInsn(NEW, "java/util/LinkedList")
    initMethod.visitInsn(DUP)
    initMethod.visitMethodInsn(INVOKESPECIAL, "java/util/LinkedList", "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)

    // Init `channelLock` field
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitTypeInsn(NEW, "java/util/concurrent/locks/ReentrantLock")
    initMethod.visitInsn(DUP)
    initMethod.visitMethodInsn(INVOKESPECIAL, "java/util/concurrent/locks/ReentrantLock", "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "channelLock", JvmType.Lock.toDescriptor)

    // Init `channelLock` field
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitTypeInsn(NEW, "java/util/ArrayList")
    initMethod.visitInsn(DUP)
    initMethod.visitMethodInsn(INVOKESPECIAL, "java/util/ArrayList", "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "conditions", JvmType.JavaList.toDescriptor)

    // Init `capacity` field
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitVarInsn(ILOAD, 1)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "capacity", JvmType.PrimInt.toDescriptor)

    // Init `channelNotFull` field
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, "channelLock", JvmType.Lock.toDescriptor)
    initMethod.visitMethodInsn(INVOKEINTERFACE, "java/util/concurrent/locks/Lock", "newCondition", AsmOps.getMethodDescriptor(Nil, JvmType.Condition), true)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "channelNotFull", JvmType.Condition.toDescriptor)

    // Init `channelNotEmpty` field
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, "channelLock", JvmType.Lock.toDescriptor)
    initMethod.visitMethodInsn(INVOKEINTERFACE, "java/util/concurrent/locks/Lock", "newCondition", AsmOps.getMethodDescriptor(Nil, JvmType.Condition), true)
    // ??? To Magnus: Why are the next two lines needed?
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitInsn(SWAP)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "channelNotEmpty", JvmType.Condition.toDescriptor)

    initMethod.visitInsn(RETURN)
    initMethod.visitMaxs(0, 2)
    initMethod.visitEnd()
  }

  def genGetValue(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val iLoad = AsmOps.getLoadInstruction(channelType)
    val iRet = AsmOps.getReturnInstruction(channelType)
    val getValue = visitor.visitMethod(ACC_PUBLIC, "getValue", AsmOps.getMethodDescriptor(Nil, channelType), null, null)
    getValue.visitCode()

    getValue.visitVarInsn(ALOAD, 0)
    getValue.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)
    getValue.visitMethodInsn(INVOKEINTERFACE, "java/util/concurrent/BlockingQueue", "take", AsmOps.getMethodDescriptor(Nil, channelType), true)
    getValue.visitInsn(iRet)
    getValue.visitMaxs(1, 1)
    getValue.visitEnd()
  }

  def genIsEmpty(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val isEmpty = visitor.visitMethod(ACC_PUBLIC, "isEmpty", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), null, null)
    isEmpty.visitCode()
    isEmpty.visitVarInsn(ALOAD, 0)
    isEmpty.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)
    isEmpty.visitMethodInsn(INVOKEINTERFACE, "java/util/LinkedList", "isEmpty", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), true)
    isEmpty.visitInsn(IRETURN)
    isEmpty.visitMaxs(1, 1)
    isEmpty.visitEnd()
  }

  def genSize(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val size = visitor.visitMethod(ACC_PUBLIC, "size", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt), null, null)
    size.visitCode()
    size.visitVarInsn(ALOAD, 0)
    size.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)
    size.visitMethodInsn(INVOKEINTERFACE, JvmType.LinkedList.name.toInternalName, "size", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt), true)
    size.visitInsn(IRETURN)
    size.visitMaxs(1, 1)
    size.visitEnd()
  }

  def genIsFull(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val isFull = visitor.visitMethod(ACC_PUBLIC, "isFull", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), null, null)
    isFull.visitCode()
    isFull.visitVarInsn(ALOAD, 0)
    isFull.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "size", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), true)
    isFull.visitVarInsn(ALOAD, 0)
    isFull.visitFieldInsn(GETFIELD, classType.name.toInternalName, "capacity", JvmType.PrimInt.toDescriptor)
    isFull.visitMethodInsn(INVOKEVIRTUAL, JvmType.Object.name.toInternalName, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), true)
    isFull.visitInsn(IRETURN)
    isFull.visitMaxs(2, 1)
    isFull.visitEnd()
  }

  def genPutValue(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val putValue = visitor.visitMethod(ACC_PUBLIC, "putValue", AsmOps.getMethodDescriptor(List(channelType), classType), null, null)
    putValue.visitCode()

    putValue.visitVarInsn(ALOAD, 0)
    putValue.visitInsn(ARETURN)
    putValue.visitEnd()
  }
}
