package gleam.compiler

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type
import java.lang.Integer

trait ByteCodePatterns {
  protected def getMethodVisitor : MethodVisitor
  private def mv = getMethodVisitor
  
  def NEW(clazz : Class[_], argClazzes : Class[_]*)(args : => Unit) : Unit = {
    NEW(Type.getInternalName(clazz), argClazzes:_*)(args)
  }
  
  def NEW(clazzName : String, argClazzes : Class[_]*)(args : => Unit) : Unit = {
    mv.visitTypeInsn(Opcodes.NEW, clazzName) // args, macro value, arg
    DUP // args, args, macro value, arg
    args
    mv.visitMethodInsn(
      INVOKESPECIAL, 
      clazzName,
      "<init>", 
      Type.getMethodDescriptor(Type.VOID_TYPE, argClazzes.map(Type.getType(_)).toArray)
    )
  }
  
  def LDC(value : String) = getMethodVisitor.visitLdcInsn(value)
  def LDC(value : Integer) = getMethodVisitor.visitLdcInsn(value)
  def DUP = getMethodVisitor.visitInsn(Opcodes.DUP)
  def POP = getMethodVisitor.visitInsn(Opcodes.POP)
  
  def INVOKE(clazz : Class[_], method : String, descriptor : String) = {
    val invokeType = if (clazz.isInterface) INVOKEINTERFACE else INVOKEVIRTUAL
    mv.visitMethodInsn(invokeType, Type.getInternalName(clazz), method, descriptor)
  }
  
  def INVOKESTATIC(clazz : Class[_], method : String, descriptor : String) = {
    mv.visitMethodInsn(Opcodes.INVOKESTATIC, Type.getInternalName(clazz), method, descriptor)
  }
  
  def getFromScope(name : String) : Unit = {
    mv.visitVarInsn(ALOAD, 1) // scope
    mv.visitLdcInsn(name) // name, scope
    mv.visitMethodInsn(INVOKEVIRTUAL, "gleam/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;")
  }
  
  def getFromScope(name : String, expectedClass : Class[_]) : Unit = {
    getFromScope(name)
    CHECKCAST(expectedClass)
  }
  
  def addToNodeListFromStack = {
    mv.visitVarInsn(ALOAD, 2) // list, node
    mv.visitInsn(SWAP) // node, list
    mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z")
    POP
  }
  
  /**
   * Adds a value with the given identifier onto the current scope. 
   * Assumes the value is on the stack already.
   */
  def addToScopeFromStack(identifier : String) = {
    mv.visitVarInsn(ALOAD, 1) // scope, string
    mv.visitInsn(SWAP) // string, scope
    mv.visitLdcInsn(identifier) // name, value, scope
    mv.visitInsn(SWAP) // value, name, scope
    mv.visitMethodInsn(INVOKEVIRTUAL, "gleam/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V")
  }
  
  /**
   * Adds a value with the given identifier onto the current scope. 
   * The value is placed on the stack from the given function.
   */
  def addToScope(identifier : String)(value : => Unit) = {
    mv.visitVarInsn(ALOAD, 1)
    mv.visitLdcInsn(identifier)
    value
    mv.visitMethodInsn(INVOKEVIRTUAL, "gleam/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V")
  }
  
  def CHECKCAST(clazz : Class[_]) = {
    getMethodVisitor.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(clazz))
  }
  
  def addAllNodesFromStack {
    mv.visitVarInsn(ALOAD, 2) // list, nodes
    mv.visitInsn(SWAP) // nodes, list
    mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "addAll", "(Ljava/util/Collection;)Z")
    POP
  }
}