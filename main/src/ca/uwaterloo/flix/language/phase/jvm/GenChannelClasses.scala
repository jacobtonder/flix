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
    AsmOps.compileField(visitor, "queue", JvmType.Queue, isStatic = false, isPrivate = true)

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

    // Generate `poll` method
    genPoll(classType, channelType, visitor)

    // Generate `offer` method
    genOffer(classType, channelType, visitor)

    // Generate `isEmpty` method
    genIsEmpty(classType, visitor)

    // Generate `isFull` method
    genIsFull(classType, visitor)

    // Generate `size` method
    genSize(classType, visitor)

    // Generate `lock` method
    genLock(classType, visitor)

    // Generate `unlock` method
    genUnlock(classType, visitor)

    // Generate `signalNotFull` method
    genSignalNotFull(classType, visitor)

    // Generate `signalNotEmpty` method
    genSignalNotEmpty(classType, visitor)

    // Generate `clearSelects` method
    genClearSelects(classType, visitor)

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

  /**
    * Generates the constructor of the channel `classType`
    */
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
    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "queue", JvmType.Queue.toDescriptor)

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

  /**
    * Generates the `getValue()` method of the `classType`
    */
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

  /**
    * Generates the `poll()` method of the `classType`
    */
  def genPoll(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val iRet = AsmOps.getReturnInstruction(channelType)
    val poll = visitor.visitMethod(ACC_PUBLIC, "poll", AsmOps.getMethodDescriptor(Nil, channelType), null, null)
    poll.visitCode()
    poll.visitVarInsn(ALOAD, 0)
    poll.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)
    poll.visitMethodInsn(INVOKEVIRTUAL, JvmType.LinkedList.name.toInternalName, "poll", AsmOps.getMethodDescriptor(Nil, channelType), false)
    poll.visitInsn(iRet)
    poll.visitMaxs(1, 1)
    poll.visitEnd()
  }

  /**
    * Generates the `offer()` method of the `classType` with value of type `channelType`
    */
  def genOffer(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val iLoad = AsmOps.getLoadInstruction(channelType)
    val offer = visitor.visitMethod(ACC_PUBLIC, "offer", AsmOps.getMethodDescriptor(List(channelType), JvmType.PrimBool), null, null)
    offer.visitCode()
    offer.visitVarInsn(ALOAD, 0)
    offer.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)
    offer.visitVarInsn(iLoad, 1)
    offer.visitMethodInsn(INVOKEVIRTUAL, JvmType.LinkedList.name.toInternalName, "offer", AsmOps.getMethodDescriptor(List(channelType), JvmType.PrimBool), false)
    offer.visitInsn(IRETURN)
    offer.visitMaxs(1, 1)
    offer.visitEnd()
  }

  /**
    * Generates the `isEmpty()` method of the `classType`
    */
  def genIsEmpty(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val isEmpty = visitor.visitMethod(ACC_PUBLIC, "isEmpty", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), null, null)
    isEmpty.visitCode()
    isEmpty.visitVarInsn(ALOAD, 0)
    isEmpty.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)
    isEmpty.visitMethodInsn(INVOKEINTERFACE, JvmType.LinkedList.name.toInternalName, "isEmpty", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), true)
    isEmpty.visitInsn(IRETURN)
    isEmpty.visitMaxs(1, 1)
    isEmpty.visitEnd()
  }

  /**
    * Generates the `isFull()` method of the `classType`
    */
  def genIsFull(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val isFull = visitor.visitMethod(ACC_PUBLIC, "isFull", AsmOps.getMethodDescriptor(Nil, JvmType.PrimBool), null, null)
    val labelElse = new Label()
    val labelEnd = new Label()
    isFull.visitCode()

    // Get the `queue` field
    isFull.visitVarInsn(ALOAD, 0)
    isFull.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)

    // Get the size of the queue
    isFull.visitMethodInsn(INVOKEINTERFACE, JvmType.LinkedList.name.toInternalName, "size", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt), true)

    // Get the `capacity` field
    isFull.visitVarInsn(ALOAD, 0)
    isFull.visitFieldInsn(GETFIELD, classType.name.toInternalName, "capacity", JvmType.PrimInt.toDescriptor)

    // Compare the size of the queue with the capacity
    isFull.visitJumpInsn(IF_ICMPNE, labelElse)

    // The size of the queue and the capacity are equal
    isFull.visitInsn(ICONST_1)
    isFull.visitJumpInsn(GOTO, labelEnd)

    // The size of the queue and the capacity are not equal
    isFull.visitLabel(labelElse)
    isFull.visitInsn(ICONST_0)

    isFull.visitLabel(labelEnd)
    isFull.visitInsn(IRETURN)
    isFull.visitMaxs(2, 2)
    isFull.visitEnd()
  }

  /**
    * Generates the `size()` method of the `classType`
    */
  def genSize(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val size = visitor.visitMethod(ACC_PUBLIC, "size", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt), null, null)
    size.visitCode()
    size.visitVarInsn(ALOAD, 0)
    size.visitFieldInsn(GETFIELD, classType.name.toInternalName, "queue", JvmType.LinkedList.toDescriptor)
    size.visitMethodInsn(INVOKEINTERFACE, JvmType.LinkedList.name.toInternalName, "size", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt), true)
    size.visitInsn(IRETURN)
    size.visitMaxs(1, 1)
    size.visitEnd()
  }

  /**
    * Generates the `lock()` method of the `classType`
    */
  def genLock(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val lock = visitor.visitMethod(ACC_PUBLIC, "lock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    lock.visitCode()
    lock.visitVarInsn(ALOAD, 0)
    lock.visitFieldInsn(GETFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)
    lock.visitMethodInsn(INVOKEINTERFACE, JvmType.Lock.name.toInternalName, "lock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)
    lock.visitInsn(RETURN)
    lock.visitMaxs(1, 1)
    lock.visitEnd()
  }

  /**
    * Generates the `unlock()` method of the `classType`
    */
  def genUnlock(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val unlock = visitor.visitMethod(ACC_PUBLIC, "unlock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    unlock.visitCode()
    unlock.visitVarInsn(ALOAD, 0)
    unlock.visitFieldInsn(GETFIELD, classType.name.toInternalName, "lock", JvmType.Lock.toDescriptor)
    unlock.visitMethodInsn(INVOKEINTERFACE, JvmType.Lock.name.toInternalName, "unlock", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)
    unlock.visitInsn(RETURN)
    unlock.visitMaxs(1, 1)
    unlock.visitEnd()
  }

  /**
    * Generates the `signalNotFull()` method of the `classType`
    */
  def genSignalNotFull(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val signalNotFull = visitor.visitMethod(ACC_PUBLIC, "signalNotFull", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    signalNotFull.visitCode()
    signalNotFull.visitVarInsn(ALOAD, 0)
    signalNotFull.visitFieldInsn(GETFIELD, classType.name.toInternalName, "channelNotFull", JvmType.Condition.toDescriptor)
    signalNotFull.visitMethodInsn(INVOKEINTERFACE, JvmType.Condition.name.toInternalName, "signalAll", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)
    signalNotFull.visitInsn(RETURN)
    signalNotFull.visitMaxs(1, 1)
    signalNotFull.visitEnd()
  }

  /**
    * Generates the `signalNotEmpty()` method of the `classType`
    */
  def genSignalNotEmpty(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val signalNotEmpty = visitor.visitMethod(ACC_PUBLIC, "signalNotEmpty", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    signalNotEmpty.visitCode()
    signalNotEmpty.visitVarInsn(ALOAD, 0)
    signalNotEmpty.visitFieldInsn(GETFIELD, classType.name.toInternalName, "channelNotEmpty", JvmType.Condition.toDescriptor)
    signalNotEmpty.visitMethodInsn(INVOKEINTERFACE, JvmType.Condition.name.toInternalName, "signalAll", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)
    signalNotEmpty.visitInsn(RETURN)
    signalNotEmpty.visitMaxs(1, 1)
    signalNotEmpty.visitEnd()
  }

  /**
    * Generates the `clearSelects()` method of the `classType`
    */
  def genClearSelects(classType: JvmType.Reference, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val clearSelects = visitor.visitMethod(ACC_PUBLIC, "clearSelects", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    clearSelects.visitCode()
    clearSelects.visitVarInsn(ALOAD, 0)
    clearSelects.visitFieldInsn(GETFIELD, classType.name.toInternalName, "selects", JvmType.JavaList.toDescriptor)
    clearSelects.visitMethodInsn(INVOKEINTERFACE, JvmType.JavaList.name.toInternalName, "clear", AsmOps.getMethodDescriptor(Nil, JvmType.Void), true)
    clearSelects.visitInsn(RETURN)
    clearSelects.visitMaxs(1, 1)
    clearSelects.visitEnd()
  }

  /**
    * Generates the `putValue()` method of the `classType` with value of type `channelType`
    */
  def genPutValue(classType: JvmType.Reference, channelType: JvmType, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val putValue = visitor.visitMethod(ACC_PUBLIC, "putValue", AsmOps.getMethodDescriptor(List(channelType), classType), null, null)
    putValue.visitCode()

    putValue.visitVarInsn(ALOAD, 0)
    putValue.visitInsn(ARETURN)
    putValue.visitEnd()
  }
}
