package uk.co.colinhowe.glimpse.compiler

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type

trait ByteCodePatterns {
  protected def getMethodVisitor : MethodVisitor
  private def mv = getMethodVisitor
  
  def getFromScope(name : String) = {
    mv.visitVarInsn(ALOAD, 1) // scope
    mv.visitLdcInsn(name) // name, scope
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;")
  }
  
  def addToNodeListFromStack = {
    mv.visitVarInsn(ALOAD, 2) // list, node
    mv.visitInsn(SWAP) // node, list
    mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z")
    mv.visitInsn(POP)
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
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V")
  }
  
  /**
   * Adds a value with the given identifier onto the current scope. 
   * The value is placed on the stack from the given function.
   */
  def addToScope(identifier : String)(value : => Unit) = {
    mv.visitVarInsn(ALOAD, 1)
    mv.visitLdcInsn(identifier)
    value
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V")
  }
  
  def CHECKCAST(clazz : Class[_]) = {
    getMethodVisitor.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(clazz))
  }
  
  
  
  def addAllNodesFromStack {
    mv.visitVarInsn(ALOAD, 2) // list, nodes
    mv.visitInsn(SWAP) // nodes, list
    mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "addAll", "(Ljava/util/Collection;)Z")
    mv.visitInsn(POP)
  }
}