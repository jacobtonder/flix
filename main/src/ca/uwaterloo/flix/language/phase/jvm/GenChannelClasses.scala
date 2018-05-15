package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import org.objectweb.asm.Label
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

object GenChannelClasses {
  /**
    * Returns the bytecode for the channel classes built-in to the Flix language.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {

    // Type that we need a channel class for
    val types = List(JvmType.PrimBool, JvmType.PrimChar, JvmType.PrimFloat, JvmType.PrimDouble,
      JvmType.PrimByte, JvmType.PrimShort, JvmType.PrimInt, JvmType.PrimLong, JvmType.Tuple,
      JvmType.Unit, JvmType.Object)

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

    // Generate the `queue` field
    AsmOps.compileField(visitor, "queue", JvmType.LinkedList, isStatic = false, isPrivate = true)

    // Generate the `lock` field
    AsmOps.compileField(visitor, "lock", JvmType.Lock, isStatic = false, isPrivate = true)

    // Generate the `capacity` field
    AsmOps.compileField(visitor, "capacity", JvmType.PrimInt, isStatic = false, isPrivate = true)

    // Generate the `channelNotFull` field
    AsmOps.compileField(visitor, "channelNotFull", JvmType.Condition, isStatic = false, isPrivate = true)

    // Generate the `channelNotEmpty` field
    AsmOps.compileField(visitor, "channelNotEmpty", JvmType.Condition, isStatic = false, isPrivate = true)

    // Generate the `selects` field
    AsmOps.compileField(visitor, "selects", JvmType.JavaList, isStatic = false, isPrivate = true)

    // Generate the constructor
    genConstructor(classType, channelType, visitor)

    // Generate `getValue` method
    //genGetValue(classType, channelType, visitor)

    // Generate `getValue` method
    //genGetChannel(classType, channelType, visitor)

    // Generate `isEmpty` method
    genIsEmpty(classType, channelType, visitor)

    // Generate `size` method
    genSize(classType, channelType, visitor)

    // Generate `lock` method
    genLock(classType, channelType, visitor)

    // Generate `unlock` method
    genUnlock(classType, channelType, visitor)

    // Generate `signalNotFull` method
    genSignalNotFull(classType, channelType, visitor)

    // Generate `signalNotEmpty` method
    genSignalNotEmpty(classType, channelType, visitor)

    // Generate `clearSelects` method
    genClearSelects(classType, channelType, visitor)

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

    // Init the `queue` field
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitTypeInsn(NEW, JvmType.LinkedList.name.toInternalName)
    initMethod.visitInsn(DUP)
    initMethod.visitMethodInsn(INVOKESPECIAL, JvmType.LinkedList.name.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)

    // Init the `lock` field
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitTypeInsn(NEW, JvmType.ReentrantLock.name.toInternalName)
    initMethod.visitInsn(DUP)
    initMethod.visitMethodInsn(INVOKESPECIAL, JvmType.ReentrantLock.name.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)

    // Init the `selects` field
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitTypeInsn(NEW, JvmType.ArrayList.name.toInternalName)
    initMethod.visitInsn(DUP)
    initMethod.visitMethodInsn(INVOKESPECIAL, JvmType.ArrayList.name.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "selects", JvmType.JavaList.toDescriptor)

    // Init `capacity` field
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitVarInsn(ILOAD, 1)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "capacity", JvmType.PrimInt.toDescriptor)

    // Init the `channelNotFull` field
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)
    initMethod.visitMethodInsn(INVOKEINTERFACE, JvmType.Lock.name.toInternalName, "newCondition", AsmOps.getMethodDescriptor(Nil, JvmType.Condition), true)
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "channelNotFull", JvmType.Condition.toDescriptor)

    // Init the `channelNotEmpty` field
    initMethod.visitVarInsn(ALOAD, 0)
    initMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)
    initMethod.visitMethodInsn(INVOKEINTERFACE, JvmType.Lock.name.toInternalName, "newCondition", AsmOps.getMethodDescriptor(Nil, JvmType.Condition), true)
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
    isEmpty.visitMethodInsn(INVOKEINTERFACE, JvmType.LinkedList.name.toInternalName, "isEmpty", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), true)
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

  def genLock(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val lock = visitor.visitMethod(ACC_PUBLIC, "lock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    lock.visitCode()
    lock.visitVarInsn(ALOAD, 0)
    lock.visitFieldInsn(GETFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)
    lock.visitMethodInsn(INVOKEINTERFACE, JvmType.Lock.name.toInternalName, "lock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)
    lock.visitInsn(RETURN)
    lock.visitMaxs(1, 1)
    lock.visitEnd()
  }

  def genUnlock(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val unlock = visitor.visitMethod(ACC_PUBLIC, "unlock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    unlock.visitCode()
    unlock.visitVarInsn(ALOAD, 0)
    unlock.visitFieldInsn(GETFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)
    unlock.visitMethodInsn(INVOKEINTERFACE, JvmType.Lock.name.toInternalName, "unlock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)
    unlock.visitInsn(RETURN)
    unlock.visitMaxs(1, 1)
    unlock.visitEnd()
  }

  def genSignalNotFull(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val signalNotFull = visitor.visitMethod(ACC_PUBLIC, "signalNotFull", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    signalNotFull.visitCode()
    signalNotFull.visitVarInsn(ALOAD, 0)
    signalNotFull.visitFieldInsn(GETFIELD, classType.name.toInternalName, "channelNotFull", JvmType.Condition.toDescriptor)
    signalNotFull.visitMethodInsn(INVOKEINTERFACE, JvmType.Condition.name.toInternalName, "signalAll", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)
    signalNotFull.visitInsn(RETURN)
    signalNotFull.visitMaxs(1, 1)
    signalNotFull.visitEnd()
  }

  def genSignalNotEmpty(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val signalNotEmpty = visitor.visitMethod(ACC_PUBLIC, "signalNotEmpty", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    signalNotEmpty.visitCode()
    signalNotEmpty.visitVarInsn(ALOAD, 0)
    signalNotEmpty.visitFieldInsn(GETFIELD, classType.name.toInternalName, "channelNotEmpty", JvmType.Condition.toDescriptor)
    signalNotEmpty.visitMethodInsn(INVOKEINTERFACE, JvmType.Condition.name.toInternalName, "signalAll", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)
    signalNotEmpty.visitInsn(RETURN)
    signalNotEmpty.visitMaxs(1, 1)
    signalNotEmpty.visitEnd()
  }

  def genClearSelects(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val clearSelects = visitor.visitMethod(ACC_PUBLIC, "clearSelects", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    clearSelects.visitCode()
    clearSelects.visitVarInsn(ALOAD, 0)
    clearSelects.visitFieldInsn(GETFIELD, classType.name.toInternalName, "selects", JvmType.JavaList.toDescriptor)
    clearSelects.visitMethodInsn(INVOKEINTERFACE, JvmType.JavaList.name.toInternalName, "clear", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)
    clearSelects.visitInsn(RETURN)
    clearSelects.visitMaxs(1, 1)
    clearSelects.visitEnd()
  }

  def genPutValue(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val putValue = visitor.visitMethod(ACC_PUBLIC, "putValue", AsmOps.getMethodDescriptor(List(channelType), classType), null, null)
    putValue.visitCode()

    putValue.visitVarInsn(ALOAD, 0)
    putValue.visitInsn(ARETURN)
    putValue.visitEnd()
  }
}
