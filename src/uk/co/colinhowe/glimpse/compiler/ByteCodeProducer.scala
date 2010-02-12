/**
 * 
 */
package uk.co.colinhowe.glimpse.compiler

import java.io.File
import java.io.FileOutputStream
import java.util.ArrayList
import java.util.HashMap
import java.util.HashSet
import java.util.LinkedList
import java.util.List
import java.util.Map
import java.util.Set
import java.util.Stack

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type

import scala.Tuple2
import scala.collection.JavaConversions
import scala.collection.JavaConversions._
import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.DynamicMacroMismatchError
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.compiler.node._
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import uk.co.colinhowe.glimpse.infrastructure.Scope



class ByteCodeProducer(
    val viewname : String, 
    val lineNumberProvider : LineNumberProvider,
    val typeResolver : TypeResolver, 
    val outputFileName : String
   ) extends DepthFirstAdapter {
  private val nodeInitMethodSignature = "(Ljava/util/List;Ljava/lang/String;Ljava/lang/Object;)V"
  
  private var generatorCount : Int = 0
  private val generatorIds = scala.collection.mutable.Map[AGenerator, Integer]()
  private val methodVisitors = new Stack[MethodVisitor]()
  private val labels = new Stack[Label]()
  private val generatorNames = new Stack[String]()
  private val classWriters = new Stack[ClassWriter]()
  private val classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS)
  private val dynamicMacros = scala.collection.mutable.Set[String]()
  private val macros = scala.collection.mutable.Set[String]()
  private var controllerType : uk.co.colinhowe.glimpse.compiler.typing.Type = null
  
  private val scopes = new Stack[Scope]()
  private val imports = scala.collection.mutable.Map[String, Class[_]]()

  // Put the first class writer onto the stack of writers
  classWriters.push(classWriter);
  
  val errors = scala.collection.mutable.Buffer[CompilationError]()
  
  override def caseAIfelse(node : AIfelse) {
    val mv = methodVisitors.peek()

    // Calculate the expression
    if(node.getExpr() != null) {
      node.getExpr().apply(this)
    }

    // Assume boolean on the stack that can be cast to a bool
    val start = new Label()
    mv.visitLabel(start)
    mv.visitTypeInsn(CHECKCAST, "java/lang/Boolean")
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z")
    
    // Jump to the end if the expression is false
    val elseLabel = new Label()
    val endLabel = new Label()
    mv.visitJumpInsn(IFEQ, elseLabel)

    // Add in the code block for truth
    if(node.getCodeblock() != null) {
      node.getCodeblock().apply(this)
      
      // Jump to the end of the if/else expression
      mv.visitJumpInsn(GOTO, endLabel)
    }
    
    // The else block begins now
    mv.visitLabel(elseLabel)
    if(node.getElse() != null) {
      node.getElse().apply(this)
    }
    mv.visitLabel(endLabel)
  }
  
  override def outAFalseExpr(node : AFalseExpr) {
    val mv = methodVisitors.peek()

    // Up-cast to a boolean
    // we don't like leave primitive types on the stack
    // This is inefficient but it simplifies implementation
    mv.visitInsn(ICONST_0)
    mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;")
    
    debug("boolean [false]")
  }
  
  override def outATrueExpr(node : ATrueExpr) {
    val mv = methodVisitors.peek()

    // Up-cast to a boolean
    // we don't like leave primitive types on the stack
    // This is inefficient but it simplifies implementation
    mv.visitInsn(ICONST_1)
    mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;")
    
    debug("boolean [true]")
  }
  
  
  override def caseAForloop(node : AForloop) {
    inAForloop(node)
    if (node.getType() != null) {
      node.getType().apply(this)
    }
    if (node.getIdentifier() != null) {
      node.getIdentifier().apply(this)
    }
    if (node.getExpr() != null) {
      node.getExpr().apply(this)
    }

    // The iterable should be on the top of the stack
    val mv = methodVisitors.peek()
    mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "iterator", "()Ljava/util/Iterator;")

    val l2 = new Label()
    mv.visitJumpInsn(GOTO, l2)

    val l3 = new Label()
    mv.visitLabel(l3)

    mv.visitInsn(DUP) // iterator, iterator
    mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;") // object, iterator

    // TODO Check the type of the expression against what we get

    val l4 = new Label()
    mv.visitLabel(l4)
    
    // Push the value on to the scope
    mv.visitVarInsn(ALOAD, 1) // scope, string, iterator
    mv.visitInsn(SWAP) // string, scope, iterator
    mv.visitLdcInsn(node.getIdentifier().getText()) // name, value, scope, iterator
    mv.visitInsn(SWAP) // value, name, scope, iterator
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V")
    
    // Now invoke the statements in between
    for (pstmt <- node.getStmt()) {
      pstmt.apply(this)
    }
    
    // iterator
    mv.visitLabel(l2)
    mv.visitInsn(DUP) // iterator, iterator
    mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z")
    mv.visitJumpInsn(IFNE, l3)
    
    // iterator
    mv.visitInsn(POP)

    outAForloop(node)
  }
  
  override def outADynamicMacroDefn(node : ADynamicMacroDefn) {
    dynamicMacros.add(node.getName().getText())
    
    // Create the class for the dynamic macro
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    
    // Get the name of the macro
    val macroName = node.getName().getText()
    
    cw.visit(V1_6, ACC_SUPER, macroName, null, "uk/co/colinhowe/glimpse/infrastructure/DynamicMacro", Array[String]())

    // Instance field for the macro
    var fv = cw.visitField(ACC_PRIVATE + ACC_STATIC, "instance", "Luk/co/colinhowe/glimpse/Macro;", null, null)
    fv.visitEnd()
    
    // Static constructor
    {
      val mv = cw.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null)
      mv.visitCode()

      // Initialise the instance
      val l0 = new Label()
      mv.visitLabel(l0)
      mv.visitTypeInsn(NEW, macroName)
      mv.visitInsn(DUP)
      mv.visitMethodInsn(INVOKESPECIAL, macroName, "<init>", "()V")
      mv.visitFieldInsn(PUTSTATIC, macroName, "instance", "Luk/co/colinhowe/glimpse/Macro;")
      
      val l1 = new Label()
      mv.visitLabel(l1)
      mv.visitInsn(RETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    } 
    
    // getInstance method
    {
      val mv = cw.visitMethod(ACC_STATIC | ACC_PUBLIC, "getInstance", "()Luk/co/colinhowe/glimpse/Macro;", null, null)
      mv.visitCode()

      val l1 = new Label()
      mv.visitLabel(l1)
      mv.visitFieldInsn(GETSTATIC, macroName, "instance", "Luk/co/colinhowe/glimpse/Macro;")
      mv.visitInsn(ARETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd();
    } 
    
    // Constructor
    {
      val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
      mv.visitCode()

      // Initialise super class
      val l0 = new Label()
      mv.visitLabel(l0)
      mv.visitVarInsn(ALOAD, 0)
      mv.visitMethodInsn(INVOKESPECIAL, "uk/co/colinhowe/glimpse/infrastructure/DynamicMacro", "<init>", "()V")
      mv.visitInsn(RETURN)
  
      val l3 = new Label()
      mv.visitLabel(l3)
      mv.visitLocalVariable("this", "L" + macroName + ";", null, l0, l3, 0)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    } 
    
//    // Invoke method - just calls through to the target macro
//    {
//      val mv = cw.visitMethod(ACC_PUBLIC, 
//          "invoke", 
//          "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Ljava/util/Map;Ljava/lang/Object;)Ljava/util/List;",
//          "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Ljava/util/Map<Ljava/lang/String;Ljava/lang/Object;>;Ljava/lang/Object;)Ljava/util/List<Luk/co/colinhowe/glimpse/Node;>;", null);
//      mv.visitCode()
//
//      mv.visitFieldInsn(GETSTATIC, macroName, "toInvoke", "Luk/co/colinhowe/glimpse/Macro;") // target
//      mv.visitVarInsn(ALOAD, 1) // scope, target
//      mv.visitVarInsn(ALOAD, 2) // args, scope, target
//      mv.visitVarInsn(ALOAD, 3) // value, args, scope, target
//      mv.visitMethodInsn(INVOKEINTERFACE, "uk/co/colinhowe/glimpse/Macro", "invoke",
//          "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Ljava/util/Map;Ljava/lang/Object;)Ljava/util/List;")
//
//      mv.visitInsn(ARETURN)
//      
//      mv.visitMaxs(4, 7)
//      mv.visitEnd()
//    }
    
    cw.visitEnd()
    
    outputClass(cw, macroName)
  }
  
  def outputClass(cw : ClassWriter, name : String) {
    val bytes = cw.toByteArray()
    
    // Output the class to a class file!
    // TODO Use some sort of factory to provide output methods
    val file = new File("temp/" + name + ".class")
//    file.deleteOnExit();
      
    val stream = new FileOutputStream(file)
    try {
      stream.write(bytes)
    } finally {
      stream.close()
    }
  }
  
  override def outAView(node : AView) {
    if (node.getStmt().size() > 0) {
      // Return the list of nodes
      val mv = methodVisitors.peek()
      val l0 = labels.peek()
      
      val l2 = new Label()
      mv.visitLabel(l2)
      mv.visitVarInsn(ALOAD, 2)
      mv.visitInsn(ARETURN)
      
      val l3 = new Label()
      mv.visitLabel(l3)
      mv.visitLocalVariable("this", "Lcheese/HelloWorld;", null, l0, l3, 0)
      
      // TODO This isn't technically accurate... the start label is too early
      mv.visitLocalVariable("scope", "Luk/co/colinhowe/glimpse/infrastructure/Scope;", null, l0, l3, 1)
      mv.visitLocalVariable("nodes", "Ljava/util/List;", "Ljava/util/List<Luk/co/colinhowe/glimpse/Node;>;", l0, l3, 2)
      mv.visitLocalVariable("controller", "Ljava/lang/Object;", null, l0, l3, 3)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
      
      classWriters.peek().visitEnd()
      
      scopes.pop()
      
      val bytes = classWriter.toByteArray()
      
      // Output the class to a class file!
      val file = new File(outputFileName)
//      file.deleteOnExit();
        
      val stream = new FileOutputStream(file)
      try {
        stream.write(bytes)
      } finally {
        stream.close()
      }
    }
  }
  
  override def outAPropertyrefExpr(node : APropertyrefExpr) {
    val path = node.getName.toString.trim.replaceAll(" ", ".")

    val l0 = new Label()
    val mv = methodVisitors.peek
    mv.visitLabel(l0)
    mv.visitTypeInsn(NEW, "uk/co/colinhowe/glimpse/PropertyReference")
    mv.visitInsn(DUP)
    
    mv.visitLdcInsn(path)
    
    // Get the value of the property
    mv.visitVarInsn(ALOAD, 1) // scope
    mv.visitLdcInsn("$controller") // name, scope
    
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;")
    mv.visitTypeInsn(CHECKCAST, getTypeName(controllerType))
    evaluateProperty(node.getName, controllerType)
    
    mv.visitMethodInsn(INVOKESPECIAL, "uk/co/colinhowe/glimpse/PropertyReference", "<init>", "(Ljava/lang/String;Ljava/lang/Object;)V")

    // Property reference is left on the stack
  }
  
  override def outAPropertyExpr(node : APropertyExpr) {
    val mv = methodVisitors.peek()
    
    // Get the value from the scope
    val l1 = new Label()
    mv.visitLabel(l1)
    
    node.getName() match {
      case simpleName : ASimpleName =>
        val name = simpleName.getIdentifier().getText()
        
        if (macros.contains(name)) {
          mv.visitMethodInsn(INVOKESTATIC, name, "getInstance", "()Luk/co/colinhowe/glimpse/Macro;") // target
          debug("simpleMacroExpr [" + simpleName.getIdentifier().getText() + "]")
        } else {
          mv.visitVarInsn(ALOAD, 1) // scope
          mv.visitLdcInsn(name) // name, scope
          mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;")
          debug("simpleExpr [" + simpleName.getIdentifier().getText() + "]")
        }
        
      case qualifiedName : AQualifiedName =>
        var node = qualifiedName
        
        val ownerName = node.getIdentifier().getText()
        
        mv.visitVarInsn(ALOAD, 1) // scope
        mv.visitLdcInsn(ownerName) // name, scope
        mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;")
        debug("propertyExpr top-level [" + ownerName + "]")

        // Cast as appropriate
        val ownerType = typeResolver.getType(node.getIdentifier(), null)
        mv.visitTypeInsn(CHECKCAST, Type.getInternalName(ownerType.asInstanceOf[SimpleType].getClazz))
        
        evaluateProperty(node.getName(), ownerType)
      
      case _ =>
        throw new RuntimeException("Unsupported type of node [" + node + "]")
    }
    
    // Leave the property on the stack for the next instruction to pick up
  }
  
  override def outAControllerPropExpr(node : AControllerPropExpr) {
    val mv = methodVisitors.peek()
    
    // Get the value from the scope
    val l1 = new Label()
    mv.visitLabel(l1)
    mv.visitVarInsn(ALOAD, 1) // scope
    
    val name = "$controller"
    mv.visitLdcInsn(name) // name, scope
    debug("controller")
    
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;")
    mv.visitTypeInsn(CHECKCAST, getTypeName(controllerType))
    evaluateProperty(node.getName(), controllerType)
  }
  
  def evaluateProperty(node : PName, ownerType : uk.co.colinhowe.glimpse.compiler.typing.Type) {
    val mv = methodVisitors.peek()
    
    // The controller will be on the stack
    val name = node match {
      case node : ASimpleName => node.getIdentifier().getText()
      case node : AQualifiedName => node.getIdentifier().getText()
      case _ => throw new RuntimeException("Type does not exist")
    }
    
    /// Determine the method return type
    var methodName = "get" + capitalise(name)
    var returnType : String = null
    var returnClass : Class[_] = null
    if (ownerType.isInstanceOf[SimpleType]) {
      returnClass = ownerType.asInstanceOf[SimpleType].clazz.getMethod(methodName).getReturnType()
      returnType = Type.getInternalName(returnClass)
    } else {
      throw new IllegalArgumentException("Only support simple types [" + ownerType + "]")
    }
    mv.visitMethodInsn(INVOKEVIRTUAL, getTypeName(ownerType), methodName, "()L" + returnType + ";")
    
    // Recurse if needed
    if (node.isInstanceOf[AQualifiedName]) {
      evaluateProperty(node.asInstanceOf[AQualifiedName].getName(), new SimpleType(returnClass));
    }
  }
  
  def capitalise(s : String ) = {
    s.substring(0, 1).toUpperCase() + s.substring(1)
  }
  
  def getTypeName(t : uk.co.colinhowe.glimpse.compiler.typing.Type) = {
    t match {
      case t : SimpleType => Type.getInternalName(t.clazz)
      case _ => throw new IllegalArgumentException("Only support simple types")
    }
  }
  
  
  override def inAView(node : AView) {
    // Output a view class only if the view contains something that isn't a macro definition
    if (node.getStmt().size() > 0) {
      val classWriter = classWriters.peek()
      classWriter.visit(V1_6, ACC_PUBLIC + ACC_SUPER, viewname, null, "uk/co/colinhowe/glimpse/View", Array[String]())
  
      // TODO Move this next to initialisation if possible
      val fv = classWriter.visitField(
          ACC_PRIVATE, "globalScope", Type.getDescriptor(classOf[Scope]), null, null)
      fv.visitEnd()
  
      // Create a scope for the view
      val scope = new Scope(null, false)
      scopes.add(scope)
      
      // Constructor
      {
        val mv = classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
        mv.visitCode()
  
        // Initialise super class
        val l0 = new Label()
        mv.visitLabel(l0)
        mv.visitVarInsn(ALOAD, 0)
        mv.visitMethodInsn(INVOKESPECIAL, "uk/co/colinhowe/glimpse/View", "<init>", "()V")
    
        // Initialise the scope
        val l1 = new Label()
        mv.visitLabel(l1)
        mv.visitVarInsn(ALOAD, 0)
        mv.visitTypeInsn(NEW, Type.getInternalName(classOf[Scope]))
        mv.visitInsn(DUP)
        mv.visitInsn(ACONST_NULL)
        mv.visitInsn(ICONST_0)
        mv.visitMethodInsn(INVOKESPECIAL, Type.getInternalName(classOf[Scope]), "<init>", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Z)V") 
        mv.visitFieldInsn(PUTFIELD, viewname, "globalScope", Type.getDescriptor(classOf[Scope]))
        
        val l2 = new Label()
        mv.visitLabel(l2)
        mv.visitInsn(RETURN)
    
        val l3 = new Label()
        mv.visitLabel(l3)
        mv.visitLocalVariable("this", "L" + viewname + ";", null, l0, l3, 0)
        mv.visitMaxs(0, 0)
        mv.visitEnd()
      } 
      
      // Create the view method
      {
        // TODO Make this use the classes... currently mess!
        val mv = classWriter.visitMethod(
            ACC_PUBLIC, "view", "(Ljava/lang/Object;)Ljava/util/List;", "(Ljava/lang/Object;)Ljava/util/List<Luk/co/colinhowe/glimpse/Node;>;", null)
        mv.visitCode()
        
        val l0 = new Label()
        labels.push(l0)
        mv.visitLabel(l0)
        mv.visitVarInsn(ALOAD, 0)
        
        // Put the controller in to register 3 so we don't have to rejig all the register accesses..
        mv.visitVarInsn(ALOAD, 1)
        mv.visitVarInsn(ASTORE, 3)
        mv.visitFieldInsn(GETFIELD, viewname, "globalScope", Type.getDescriptor(classOf[Scope]))
        mv.visitVarInsn(ASTORE, 1)
        
        val l1 = new Label();
        mv.visitLabel(l1);
        mv.visitTypeInsn(NEW, "java/util/LinkedList");
        mv.visitInsn(DUP);
        mv.visitMethodInsn(INVOKESPECIAL, "java/util/LinkedList", "<init>", "()V");
        mv.visitVarInsn(ASTORE, 2);
        
        // TODO Stuff goes here
        methodVisitors.push(mv);
      }
    }
  }
  
  
  
  def upperFirst(other : String) = {
    other.substring(0, 1).toUpperCase() + other.substring(1)
  }
  
  def nameToString(node : PName) = processName(node, (s : String) => s)
  def nameToStringWithGets(node : PName) = processName(node, ".get" + upperFirst(_) + "()")

  def processName(node : PName, mapper : String => String) = {
      // Chunk the name down
    var nameNode = node
    var name = ""
    
    while (nameNode != null) {
      nameNode match {
        case node : AQualifiedName =>
          name = "." + mapper(node.getIdentifier().getText()) + name
          nameNode = node.getName()
        case node : ASimpleName =>
          name = mapper(node.getIdentifier().getText()) + name
          nameNode = null
      }
    }
    name
  }

  
  def getStringFromExpr(expr : PExpr) = {
    expr match {
      case expr : AConstantExpr => expr.getNumber().getText()
      case _ => "0"
    }
  }
  

  override def caseTString(node : TString) {
    
    val sep = "\"\""
    
    if (node.getText().startsWith(sep)) {
      val lines = node.getText().split("\n")
      
      // Determine indentation from the last line - and then 2 spaces more
      val indentation = lines(lines.length - 1).indexOf(sep) + 2
      
      // Remove the indentation from each line and build up a string to output
      val outputStringBuffer = new StringBuffer()
      
      for (line <- lines.slice(1, lines.length - 1)) {
        outputStringBuffer.append(line.substring(indentation) + "\n")
      }
      
      // Remove the trailing + and quote and new line
      val text = outputStringBuffer.substring(0, outputStringBuffer.length() - 1)
      node.setText(text)
    } else {
      node.setText(node.getText().replaceAll("\"", ""))
    }
    
    // Put the text onto the stack
    val mv = methodVisitors.peek()
    mv.visitLdcInsn(node.getText())
    
    debug("string [" + node.getText() + "]")
  }
  
  private def debug(debugText : String) {
    System.out.println("[debug:" + methodVisitors.peek() + "] " + debugText)
  }


  override def outAController(node : AController) {
    // The controller will be in register 3... put it on the environment
    val mv = methodVisitors.peek()
    mv.visitVarInsn(ALOAD, 1)
    mv.visitLdcInsn("$controller")
    mv.visitVarInsn(ALOAD, 3)
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V")
    
    val className = node.getName().toString().trim().replaceAll(" ", ".")
    debug("controller has type [" + className + "]")
    
    // Load the controller class
    val clazzName = nameToStringForwards(node.getName())
    controllerType = new SimpleType(getTypeByName(clazzName))
  }

  
  override def inAMacroDefn(node : AMacroDefn) {
    
    // Create a scope for this macro
    val scope = new Scope(if(scopes.empty()) null else scopes.peek(), true)
    scopes.add(scope)
    
    // TODO Macros really should be ripped out into their own ASTs and processed separately
    
    // Create a new top level class
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    classWriters.push(cw)
    
    // Get the name of the macro
    val macroName = node.getName().getText()
    macros.add(macroName)
    
    // Create the signature for the macro
    val args = scala.collection.mutable.Set[(String, String)]()
    for (parg <- node.getArgDefn()) {
      val arg = parg.asInstanceOf[AArgDefn]
      args.add((arg.getIdentifier().toString(), arg.getType().toString()))
    }
    
    cw.visit(V1_6, ACC_SUPER, macroName, null, "java/lang/Object", Array[String]("uk/co/colinhowe/glimpse/Macro"))

    // Instance field for the macro
    val fv = cw.visitField(ACC_PRIVATE + ACC_STATIC, "instance", "Luk/co/colinhowe/glimpse/Macro;", null, null)
    fv.visitEnd()
    
    // Static constructor
    {
      val mv = cw.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null)
      mv.visitCode()

      // Initialise the instance
      val l0 = new Label()
      mv.visitLabel(l0)
      mv.visitTypeInsn(NEW, macroName)
      mv.visitInsn(DUP)
      mv.visitMethodInsn(INVOKESPECIAL, macroName, "<init>", "()V")
      mv.visitFieldInsn(PUTSTATIC, macroName, "instance", "Luk/co/colinhowe/glimpse/Macro;")
      
      val l1 = new Label()
      mv.visitLabel(l1)
      mv.visitInsn(RETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd(); 
    } 
    
    // getInstance method
    {
      val mv = cw.visitMethod(ACC_STATIC | ACC_PUBLIC, "getInstance", "()Luk/co/colinhowe/glimpse/Macro;", null, null)
      mv.visitCode()

      val l1 = new Label()
      mv.visitLabel(l1)
      mv.visitFieldInsn(GETSTATIC, macroName, "instance", "Luk/co/colinhowe/glimpse/Macro;")
      mv.visitInsn(ARETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd(); 
    } 
    
    // Constructor
    {
      val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
      mv.visitCode()

      // Initialise super class
      val l0 = new Label()
      mv.visitLabel(l0)
      mv.visitVarInsn(ALOAD, 0)
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V")
      mv.visitInsn(RETURN)
  
      val l3 = new Label()
      mv.visitLabel(l3)
      mv.visitLocalVariable("this", "L" + macroName + ";", null, l0, l3, 0)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    } 
    
    
    // Invoke method
    {
      val valueName = node.getContentName().getText()
      
      val mv = cw.visitMethod(ACC_PUBLIC, 
          "invoke", 
          "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Ljava/util/Map;Ljava/lang/Object;)Ljava/util/List;",
          "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Ljava/util/Map<Ljava/lang/String;Ljava/lang/Object;>;Ljava/lang/Object;)Ljava/util/List<Luk/co/colinhowe/glimpse/Node;>;", null)
      methodVisitors.push(mv)
      mv.visitCode()

      // TODO This or something similar could be in a parent class...
      
      // Variables
      // 0 -> this
      // 1 -> caller scope
      // 2 -> argument map
      // 3 -> content
      // 4 -> macro scope
      

      // Create a scope for the macro
      val l0 = new Label()
      mv.visitLabel(l0)
      mv.visitTypeInsn(NEW, "uk/co/colinhowe/glimpse/infrastructure/Scope")
      mv.visitInsn(DUP)
      mv.visitVarInsn(ALOAD, 1)
      mv.visitInsn(ICONST_1)
      mv.visitMethodInsn(INVOKESPECIAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "<init>", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Z)V")
      mv.visitVarInsn(ASTORE, 4)
      
      val l1 = new Label()
      mv.visitLabel(l1)
      mv.visitVarInsn(ALOAD, 2)
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Map", "entrySet", "()Ljava/util/Set;")
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Set", "iterator", "()Ljava/util/Iterator;")
      mv.visitVarInsn(ASTORE, 6)
      
      val l2 = new Label()
      mv.visitJumpInsn(GOTO, l2)
      
      val l3 = new Label()
      mv.visitLabel(l3)
      mv.visitFrame(F_FULL, 7, 
          Array[Object](macroName, "uk/co/colinhowe/glimpse/infrastructure/Scope", "java/util/Map", "java/lang/Object", "uk/co/colinhowe/glimpse/infrastructure/Scope", TOP, "java/util/Iterator"), 
          0, Array[Object]())
      mv.visitVarInsn(ALOAD, 6)
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;")
      mv.visitTypeInsn(CHECKCAST, "java/util/Map$Entry")
      mv.visitVarInsn(ASTORE, 5)
      
      val l4 = new Label()
      mv.visitLabel(l4)
      mv.visitVarInsn(ALOAD, 4)
      mv.visitVarInsn(ALOAD, 5)
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Map$Entry", "getKey", "()Ljava/lang/Object;")
      mv.visitTypeInsn(CHECKCAST, "java/lang/String")
      mv.visitVarInsn(ALOAD, 5)
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Map$Entry", "getValue", "()Ljava/lang/Object;")
      mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V")

      mv.visitLabel(l2)
      mv.visitFrame(F_SAME, 0, null, 0, null)
      mv.visitVarInsn(ALOAD, 6)
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z")
      mv.visitJumpInsn(IFNE, l3)
      
      // Put the content onto the scope
      mv.visitVarInsn(ALOAD, 4) // scope
      mv.visitLdcInsn(valueName) // name, scope
      mv.visitVarInsn(ALOAD, 3) // value, name, scope
      mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V")
      
      val l6 = new Label()
      mv.visitLabel(l6)
      mv.visitLocalVariable("this", "Lcheese/HelloWorld;", null, l0, l6, 0)
      mv.visitLocalVariable("callerScope", "Luk/co/colinhowe/glimpse/infrastructure/Scope;", null, l0, l6, 1)
      mv.visitLocalVariable("_args", "Ljava/util/Map;", "Ljava/util/Map<Ljava/lang/String;Ljava/lang/Object;>;", l0, l6, 2)
      mv.visitLocalVariable("value", "Ljava/lang/String;", null, l0, l6, 3)
      mv.visitLocalVariable("scope", "Luk/co/colinhowe/glimpse/infrastructure/Scope;", null, l1, l6, 4)
      mv.visitLocalVariable("entry", "Ljava/util/Map$Entry;", "Ljava/util/Map$Entry<Ljava/lang/String;Ljava/lang/Object;>;", l4, l2, 5)
    }
    
  }
  
  
  override def outAMacroDefn(node : AMacroDefn) {
    // Remove the scope
    scopes.pop()
    
    val mv = methodVisitors.pop()
    
    {
      // The generator will be on the stack
      val generatorId = generatorIds.get(node.getGenerator())
      val generatorIdentifier = viewname + "$$generator" + generatorId

      // generator
      mv.visitVarInsn(ALOAD, 4) // scope, generator
      mv.visitMethodInsn(INVOKEVIRTUAL, generatorIdentifier, "view", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;)Ljava/util/List;")
      // nodes
  
      mv.visitInsn(ARETURN)
  
      mv.visitMaxs(4, 7)
      mv.visitEnd()
    }
    
    val cw = classWriters.pop()
    cw.visitEnd()

    val macroName = node.getName().getText()
    outputClass(cw, macroName)
  }
  
  override def outAIncludeStmt(node : AIncludeStmt) {
    val mv = methodVisitors.peek()
    
    val include = node.getIncludeA().asInstanceOf[AIncludeA]

    // Create a new scope for the generator
    mv.visitTypeInsn(NEW, Type.getInternalName(classOf[Scope]))
    mv.visitInsn(DUP)
    mv.visitVarInsn(ALOAD, 1)
    mv.visitInsn(ICONST_0)
    mv.visitMethodInsn(INVOKESPECIAL, Type.getInternalName(classOf[Scope]), "<init>", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Z)V"); 
    
    // Populate the scope with any arguments
    // The arguments will be on the stack like so: scope, name, value, name, value

    // For each argument we need to transform the stack to be like:
    //   value, name, scope, scope, ...
    
    // Starts as scope, name, value, name, value
    // Process the arguments in reverse order as the values
    // will have been added on to the stack in forward order
    val reverseArguments = include.getArguments.reverse

    for (pargument <- reverseArguments) {
      // TODO Check types
      mv.visitInsn(DUP_X1) // scope, value, scope, ...
      mv.visitInsn(DUP_X1) // scope, value, scope, scope, ...
      mv.visitInsn(POP) // value, scope, scope, ...
      
      val arg = pargument.asInstanceOf[AArgument]
      mv.visitLdcInsn(arg.getIdentifier().getText())
      // name, value, scope, scope, ...
      mv.visitInsn(SWAP) // value, name, scope, scope, ...

      mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V")
    }
    // Ends with the scope at the top of the stack

    val generatorArgumentName = include.getTheInclude().getText()
    
    // Pull the generator from the scope
    val l1 = new Label()
    mv.visitLabel(l1)
    mv.visitVarInsn(ALOAD, 1) // scope
    mv.visitLdcInsn(generatorArgumentName) // name, scope
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;")
    
    // The generator will be on the stack
    mv.visitTypeInsn(CHECKCAST, "uk/co/colinhowe/glimpse/Generator")

    // invoke the generator
    mv.visitInsn(SWAP)
    mv.visitMethodInsn(INVOKEINTERFACE, "uk/co/colinhowe/glimpse/Generator", "view", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;)Ljava/util/List;")

    // Nodes now sit on the stack
    mv.visitVarInsn(ALOAD, 2) // list, nodes
    mv.visitInsn(SWAP)
    mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "addAll", "(Ljava/util/Collection;)Z")
    mv.visitInsn(POP)
  }

  override def inAGenerator(node : AGenerator) {
    
    // Create a scope for this generator
    val scope = new Scope(scopes.peek(), false)
    scopes.add(scope)
    
    val outerClassWriter = classWriters.peek()
    
    // Get the ID for the generator
    val id = generatorCount
    generatorCount += 1
    val generatorName = "$generator" + id

    generatorNames.push(generatorName)
    
    outerClassWriter.visitInnerClass(viewname + "$" + generatorName, viewname, generatorName, ACC_PRIVATE + ACC_STATIC)

    val innerClassWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    classWriters.push(innerClassWriter)

    innerClassWriter.visit(V1_6, ACC_SUPER, viewname + "$" + generatorName, null, "java/lang/Object", Array[String]("uk/co/colinhowe/glimpse/Generator"))
    innerClassWriter.visitInnerClass(viewname + "$" + generatorName, viewname, generatorName, ACC_PRIVATE + ACC_STATIC)

    generatorIds.put(node, id)

    // Constructor
    {
      val mv = innerClassWriter.visitMethod(ACC_PRIVATE, "<init>", "()V", null, null)
      mv.visitCode()

      // Initialise super class
      val l0 = new Label()
      mv.visitLabel(l0)
      mv.visitVarInsn(ALOAD, 0)
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V")
      mv.visitInsn(RETURN)
  
      val l3 = new Label()
      mv.visitLabel(l3)
      mv.visitLocalVariable("this", "L" + viewname + "$" + generatorName + ";", null, l0, l3, 0)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    } 
    
    // Inner constructor
    {
      val mv = innerClassWriter.visitMethod(ACC_SYNTHETIC, "<init>", "(L" + viewname + "$" + generatorName + ";)V", null, null)
      mv.visitCode()
      val l0 = new Label()
      mv.visitLabel(l0)
      mv.visitLineNumber(34, l0)
      mv.visitVarInsn(ALOAD, 0)
      mv.visitMethodInsn(INVOKESPECIAL, viewname + "$" + generatorName, "<init>", "()V")
      mv.visitInsn(RETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    }
    
    // Create the view method
    {
      // TODO Make this use the classes... currently mess!
      val mv = innerClassWriter.visitMethod(
          ACC_PUBLIC, "view", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;)Ljava/util/List;", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;)Ljava/util/List<Luk/co/colinhowe/glimpse/Node;>;", null)
      mv.visitCode()
      
      val l1 = new Label()
      mv.visitLabel(l1)
      mv.visitTypeInsn(NEW, "java/util/LinkedList")
      mv.visitInsn(DUP)
      mv.visitMethodInsn(INVOKESPECIAL, "java/util/LinkedList", "<init>", "()V")
      mv.visitVarInsn(ASTORE, 2)
      
      // TODO Stuff goes here
      methodVisitors.push(mv)
      labels.push(l1)
    }
  }

  override def outAGenerator(node : AGenerator) {
    scopes.pop()
    
    // Return the list of nodes
    var mv = methodVisitors.pop()
    val l0 = labels.pop()
    
    val l2 = new Label()
    mv.visitLabel(l2)
    mv.visitVarInsn(ALOAD, 2)
    mv.visitInsn(ARETURN)
    
    val l3 = new Label()
    mv.visitLabel(l3)
    
    val generatorName = generatorNames.pop()
    
    mv.visitLocalVariable("this", "L" + viewname + "$" + generatorName + ";", null, l0, l3, 0)
    
    // TODO This isn't technically accurate... the start label is too early
    mv.visitLocalVariable("scope", "Luk/co/colinhowe/glimpse/infrastructure/Scope;", null, l0, l3, 1)
    mv.visitLocalVariable("nodes", "Ljava/util/List;", "Ljava/util/List<Luk/co/colinhowe/glimpse/Node;>;", l0, l3, 2)
    mv.visitMaxs(0, 0)
    mv.visitEnd()
    
    val cw = classWriters.pop()
    cw.visitEnd()
    
    // Write the bytes out
    val generatorIdentifier = viewname + "$" + generatorName
    outputClass(cw, generatorIdentifier)
    
    // Create an instance of the generator
    mv = methodVisitors.peek()
    mv.visitTypeInsn(NEW, generatorIdentifier)
    mv.visitInsn(DUP) // generator, generator, args
    mv.visitInsn(ACONST_NULL) // null, generator, generator, args
    mv.visitMethodInsn(INVOKESPECIAL, generatorIdentifier, "<init>", "(L" + generatorIdentifier + ";)V")
    // generator, args    
    
    debug("generator [" + generatorIdentifier + "]")
  }

  override def outAInvertExpr(node : AInvertExpr) {
    // Assume that a Boolean is on the stack
    val mv = methodVisitors.peek()
    mv.visitTypeInsn(CHECKCAST, "java/lang/Boolean")
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z")
    mv.visitLdcInsn(1)
    mv.visitInsn(IXOR) // 1 ^ x is equivalent to !x
    mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;")
  }
  
  override def outAVarDefn(node : AVarDefn) {
    val mv = methodVisitors.peek()

    val varname = node.getIdentifier().getText()
    if (node.getExpr() != null) {
      val l1 = new Label()
      mv.visitLabel(l1)
      mv.visitVarInsn(ALOAD, 1)
      mv.visitInsn(SWAP) // Value will already be on the stack, so swap it with the scope
      mv.visitLdcInsn(varname)
      mv.visitInsn(SWAP) // value, name, scope
  
      mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V")
    }
    
    // Put this variable and the type of it on to the scope
    scopes.peek().add(varname, typeResolver.getType(node, null))
  }

  override def outAIncrementStmt(node : AIncrementStmt) {
    
    // Get the value from the scope
    val mv = methodVisitors.peek()
    mv.visitVarInsn(ALOAD, 1) //scope
    mv.visitInsn(DUP) // scope, scope
    mv.visitLdcInsn(node.getIdentifier().getText()) // name, scope, scope
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;")
    // value, scope
    mv.visitTypeInsn(CHECKCAST, "java/lang/Integer")
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I")
    mv.visitInsn(ICONST_1) // 1, value, scope
    mv.visitInsn(IADD) // value+1, scope
    mv.visitLdcInsn(node.getIdentifier().getText()) // name, value+1, scope
    mv.visitInsn(SWAP) // value+1, name, scope
    mv.visitTypeInsn(NEW, "java/lang/Integer") // i, value+1, name, scope
    mv.visitInsn(DUP_X1) // i, value+1, i, name, scope
    mv.visitInsn(SWAP) // value+1, i, i, name, scope
    mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V")
    // i, name, scope

    mv.visitTypeInsn(CHECKCAST, "java/lang/Integer")
    
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "replace", "(Ljava/lang/String;Ljava/lang/Object;)V")
  }

  
  override def outAAssignmentStmt(node : AAssignmentStmt) {
    val mv = methodVisitors.peek()
    
    // This could be a dynamic macro assignment
    val destinationVariable = node.getIdentifier().getText()
    
    if (dynamicMacros.contains(destinationVariable)) {
      debug("setting dynamic macro [" + destinationVariable + "]")
      mv.visitMethodInsn(INVOKESTATIC, destinationVariable, "getInstance", "()Luk/co/colinhowe/glimpse/Macro;") // target
      mv.visitTypeInsn(CHECKCAST, "uk/co/colinhowe/glimpse/infrastructure/DynamicMacro")
      mv.visitInsn(SWAP)
      mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/DynamicMacro", "setToInvoke", "(Luk/co/colinhowe/glimpse/Macro;)V")
    } else {
      // The value will be sitting on the stack
      mv.visitVarInsn(ALOAD, 1) // scope, value
      mv.visitInsn(SWAP) // value, scope
  
      mv.visitLdcInsn(node.getIdentifier().getText()) // name, scope, value
      mv.visitInsn(SWAP) // value, name, scope
      mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "replace", "(Ljava/lang/String;Ljava/lang/Object;)V")
    }
  }
  
  override def outAConstantExpr(node : AConstantExpr) {
    val mv = methodVisitors.peek()
    val i = Integer.valueOf(node.getNumber().getText())
    mv.visitLdcInsn(i)

    // Up-cast to an Integer
    // we don't like leave primitive types on the stack
    // This is inefficient but it simplifies implementation
    mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;")
    
    debug("constant [" + i + "]")
  }

  override def outAMacroStmt(node : AMacroStmt) {
    val invocation = node.getMacroInvoke().asInstanceOf[AMacroInvoke]
    val mv = methodVisitors.peek()
    
    // The arguments will be on the stack already.
    // The stack will look like:
    //   macro value, arg, arg, ...
    
    // Put all the arguments into a map
    mv.visitTypeInsn(NEW, "java/util/HashMap") // args, macro value, arg
    mv.visitInsn(DUP) // args, args, macro value, arg
    mv.visitMethodInsn(INVOKESPECIAL, "java/util/HashMap", "<init>", "()V")
    // args, macro value, arg
    mv.visitTypeInsn(CHECKCAST, "java/util/Map") // args, macro value, arg

    val args = invocation.getArguments()
    
    for (pargument <- args) {
      val argument = pargument.asInstanceOf[AArgument]

      // Shuffle the stack around so the macro value and args are at the bottom

      // args, macro value, arg
      mv.visitInsn(SWAP) // macro value, args, arg
      mv.visitInsn(DUP2_X1) // macro value, args, arg, macro value, args
      mv.visitInsn(POP) // args, arg, macro value, args
      mv.visitInsn(SWAP) // arg, args, macro value, args
      
      // Put the variable name on
      mv.visitLdcInsn(argument.getIdentifier().getText())
      // name, arg, args, macro value, args
      mv.visitInsn(SWAP) // arg, name, args, macro value, args
      
      // Put the variable on the arguments
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Map", "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;")
      // object, macro value, args
      mv.visitInsn(POP) // macro value, args
      mv.visitInsn(SWAP) // args, macro value
    }

    val macroName = invocation.getIdentifier().getText()
    
    // Stack: args, value (string)
    mv.visitInsn(SWAP) // value, args

    debug("macro invokation [" + macroName + "]")
    mv.visitMethodInsn(INVOKESTATIC, macroName, "getInstance", "()Luk/co/colinhowe/glimpse/Macro;") // macro, value, args
    
    // macro, value, args
    mv.visitInsn(DUP_X2) // macro, value, args, macro
    mv.visitInsn(POP) // value, args, macro

    mv.visitVarInsn(ALOAD, 1) // scope, value, args, macro
    mv.visitInsn(DUP_X2) // scope, value, args, scope, macro
    mv.visitInsn(POP) // value, args, scope, macro

    mv.visitTypeInsn(CHECKCAST, "java/lang/Object") // value, args, scope, macro
    mv.visitMethodInsn(INVOKEINTERFACE, "uk/co/colinhowe/glimpse/Macro", "invoke", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Ljava/util/Map;Ljava/lang/Object;)Ljava/util/List;")
    
    // nodes
    mv.visitVarInsn(ALOAD, 2) // list, nodes
    mv.visitInsn(SWAP) // nodes, list
    mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "addAll", "(Ljava/util/Collection;)Z")
    mv.visitInsn(POP)
  }
  
  
  /**
   * When we come in to this method we expect the following stack:
   *   value
   *   property value -- Property pairs
   *   property name  -|
   */
  override def outANodeCreate(node : ANodeCreate) {
    val id = node.getId().getText()
    val mv = methodVisitors.peek()
    
    // Start creating the node
    // Load up the node list ready for adding the node to
    // Create the node on the stack ready for setting properties on it
    val l1 = new Label()
    mv.visitLabel(l1)
//    mv.visitVarInsn(ALOAD, 2) // list, value
      
    mv.visitTypeInsn(NEW, "uk/co/colinhowe/glimpse/Node") // node, value
    mv.visitInsn(DUP_X1) // node, value, node
    mv.visitInsn(SWAP) // value, node, node
      
    // TODO Expression evaluation should be done way better!
    if (node.getExpr().isInstanceOf[AGeneratorExpr]) {
      // TODO Create the generator object :)
      val generatorExp = node.getExpr().asInstanceOf[AGeneratorExpr]

      val generatorIdentifier = viewname + "$$generator" + generatorIds.get(generatorExp.getGenerator())

      // Stack: generator, node, node
      mv.visitVarInsn(ALOAD, 1) // scope, generator, node, node
      mv.visitMethodInsn(INVOKEVIRTUAL, generatorIdentifier, "view", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;)Ljava/util/List;")
      // nodes, node, node
      mv.visitLdcInsn(id);  // id, nodes, node, node
      mv.visitInsn(ACONST_NULL) // null, id, nodes, node, node
      mv.visitMethodInsn(INVOKESPECIAL, "uk/co/colinhowe/glimpse/Node", "<init>", nodeInitMethodSignature)
      // node

      // Set properties on the node
      addParameters(mv, node)
      
      mv.visitVarInsn(ALOAD, 2) // list, node
      mv.visitInsn(SWAP) // node, list
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z")
      mv.visitInsn(POP)
    } else {
      
      /*
       * Output bytecode equivalent to:
       *   Node n = new Node(null, "\"" + id + "\", \"" + text + "\"")
       *   nodes.add(n)
       */
      
      // TODO Store the node off in a local variable, n
      // Then visitLocalVariable in this method.
      // This is fine as we can limit scope of the variable using labels and all is good
      
      // The value is on the top of the stack
      val l2 = new Label()
      mv.visitLabel(l2)
//      mv.visitTypeInsn(NEW, "uk/co/colinhowe/glimpse/Node")
//      mv.visitInsn(DUP_X1) // node, value, node
//      mv.visitInsn(SWAP) // value, node, node
      mv.visitInsn(ACONST_NULL) // null, value, node, node
      mv.visitInsn(SWAP) // value, null, node, node
      mv.visitLdcInsn(id) // id, value, null, node, node
      mv.visitInsn(SWAP) // value, id, null, node, node
      mv.visitMethodInsn(INVOKESPECIAL, "uk/co/colinhowe/glimpse/Node", "<init>", nodeInitMethodSignature)
      // node
      
      // Set properties on the node
      addParameters(mv, node)
      
      // node
      
      mv.visitVarInsn(ALOAD, 2) // list, node
      mv.visitInsn(SWAP) // node, list
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z")
      // boolean
      mv.visitInsn(POP)
    }
  }
  
  
  /**
   * Assume that the node is already on the stack and that the node must remain
   * on the stack afterwards.
   * 
   * @param mw
   * @param node
   */
  def addParameters(mv : MethodVisitor, node : ANodeCreate) {
    // Stack starts like this:
    // node, values...

    // Add the parameters on
    // In reverse order as the expressions are added on to 
    // the call in order which inverts them (due to it being a stack)
    val reverseArguments = node.getArguments.reverse

    for (_argument <- reverseArguments) {
      val argument = _argument.asInstanceOf[AArgument]

      val name = argument.getIdentifier().getText()
      
      // node, value
      mv.visitInsn(DUP_X1) // node, value, node
      mv.visitInsn(SWAP) // value, node, node
      mv.visitLdcInsn(name) // name, value, node, node
      mv.visitInsn(SWAP) // value, name, node, node
      mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/Node", "setAttribute", "(Ljava/lang/String;Ljava/lang/Object;)V")
    }
  }

  // TODO Move this out into a type name resolver
  override def outAImport(node : AImport) {
    val qualifiedName = nameToStringForwards(node.getName())
    val clazzName = nameToClazzName(node.getName())
    
    imports(clazzName) = this.getClass().getClassLoader().loadClass(qualifiedName)
  }
  
  def nameToStringForwards(name : PName) : String = {
    name.toString().trim().replaceAll(" ", ".")
  }
  
  def getTypeByName(clazzName : String) : Class[_] = {
    // Check to see if there is an import for this class name
    // We don't have to worry about periods as they won't be in the set of
    // imports anyway
    imports.get(clazzName) match {
      case Some(clazz) => clazz
      case None => this.getClass().getClassLoader().loadClass(clazzName)
    }
  }
  
  def nameToClazzName(node : PName) : String = {
    node match {
      case node : AQualifiedName => nameToClazzName(node.getName)
      case node : ASimpleName => node.getIdentifier.getText
    }
  }
  
}