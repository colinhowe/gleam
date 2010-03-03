package uk.co.colinhowe.glimpse.compiler

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type

trait ByteCodePatterns {
  protected def getMethodVisitor : MethodVisitor
  
  def getFromScope(name : String) = {
    val mv = getMethodVisitor
    mv.visitVarInsn(ALOAD, 1) // scope
    mv.visitLdcInsn(name) // name, scope
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;")
  }
  
  def CHECKCAST(clazz : Class[_]) = {
    getMethodVisitor.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(clazz))
  }
}