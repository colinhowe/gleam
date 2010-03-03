/**
 * 
 */
package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.PropertyReference
import uk.co.colinhowe.glimpse.infrastructure.DynamicMacro
import uk.co.colinhowe.glimpse.Generator
import uk.co.colinhowe.glimpse.compiler.node.PModifier
import java.util.LinkedList
import uk.co.colinhowe.glimpse.compiler.typing.GenericType
import java.io.File
import java.io.FileOutputStream

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type

import scala.Tuple2
import scala.collection.JavaConversions
import scala.collection.JavaConversions._
import scala.collection.mutable.{ Stack => MStack, Set => MSet, Map => MMap }
import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.DynamicMacroMismatchError
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.compiler.node._
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import uk.co.colinhowe.glimpse.infrastructure.Scope
import uk.co.colinhowe.glimpse.compiler.IdentifierConverter._

import uk.co.colinhowe.glimpse.compiler.typing.{ Type => GType }

import ArgumentSource._

class ByteCodeProducer(
    val viewname : String, 
    val lineNumberProvider : LineNumberProvider,
    val typeResolver : TypeResolver, 
    val outputFileName : String,
    implicit val typeNameResolver : TypeNameResolver,
    val sourcename : String,
    val callResolver : CallResolver,
    val resolvedCallsProvider : ResolvedCallsProvider
   ) extends DepthFirstAdapter with Conversions with ByteCodePatterns {
  private implicit val typeProvider = typeResolver.typeProvider
    
  private var generatorCount : Int = 0
  private val generatorIds = scala.collection.mutable.Map[AGenerator, Integer]()
  private val methodVisitors = new MStack[MethodVisitor]()
  private val labels = new MStack[Label]()
  private val generatorNames = new MStack[String]()
  private val classWriters = new MStack[ClassWriter]()
  private val classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS)
  private val dynamicMacros = scala.collection.mutable.Set[String]()
  private val macros = scala.collection.mutable.Set[String]()
  private var inMacro = false
  private val controllers = new MStack[Class[_]]()
  controllers.push(null)

  private val methodResolver = new MethodResolver(typeResolver, typeNameResolver)
  
  private val scopes = new MStack[Scope]()

  // Put the first class writer onto the stack of writers
  classWriters.push(classWriter);
  
  val errors = scala.collection.mutable.Buffer[CompilationError]()
  val labelsWithLines = MSet[Label]()
  val trailingLabels = new MStack[Option[Label]]()
  
  protected def getMethodVisitor = { 
    methodVisitors.head 
  }
  
  // TODO Refactor out into a seperate class
  // TODO write some tests for this 
  private def startLabel(node : Node) : Label = {
    val label = new Label()
    val mv = methodVisitors.head
    if (node != null) {
      mv.visitLabel(label)
      setLineNumber(mv, label, lineNumberProvider.getLineNumber(node))
      labelsWithLines.add(label)
      trailingLabels.pop
      trailingLabels.push(None)
    }
    label
  }
  
  private def setLineNumber(mv : MethodVisitor, label : Label, line : Option[Int]) {
    line match {
      case Some(line) =>
        if (!labelsWithLines.contains(label)) {
          mv.visitLineNumber(line, label)
          labelsWithLines.add(label)
        }
      case _ =>
    }
  }
  
  private def startOrContinueLabel(node : Node) : Label = {
    val (label, continued) = trailingLabels.pop match {
      case Some(label) => (label, true)
      case _ => (new Label(), false)
    }
    val mv = methodVisitors.head
    if (!continued) {
      mv.visitLabel(label)
    }
    setLineNumber(mv, label, lineNumberProvider.getLineNumber(node))
    trailingLabels.push(None)
    label
  }

  
  private def visitLabel(label : Label) {
    val mv = methodVisitors.head
    mv.visitLabel(label)
    trailingLabels.pop
    trailingLabels.push(Some(label))
  }
  
  override def caseAControllerMethodExpr(node : AControllerMethodExpr) {
    val mv = methodVisitors.head
  
    val method = methodResolver.getMatchingMethod(controllers.head, node).get
    
    val invokeType = if (controllers.head.isInterface) INVOKEINTERFACE else INVOKEVIRTUAL

    getFromScope("$controller")
    CHECKCAST(controllers.head)
    
    // Load the expressions onto the stack
    node.getExpr.zip(method.getParameterTypes()).foreach { case(expr, clazz) => 
      expr.apply(this) 
      CHECKCAST(clazz)
    }
    
    mv.visitMethodInsn(
      invokeType,
      Type.getInternalName(controllers.head),
      node.getIdentifier.getText,
      Type.getMethodDescriptor(method)
    )
    
  }
  
  override def caseAIfelse(node : AIfelse) {
    val mv = methodVisitors.head

    // Calculate the expression
    val start = startOrContinueLabel(node)
    if(node.getExpr() != null) {
      node.getExpr().apply(this)
    }

    // Assume boolean on the stack that can be cast to a bool
    CHECKCAST(classOf[java.lang.Boolean])
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z")
    
    // Jump to the end if the expression is false
    val endLabel = startLabel(null)
    val elseLabel = if (node.getElse() != null) startLabel(null) else endLabel
    mv.visitJumpInsn(IFEQ, elseLabel)
    
    // Add in the code block for truth
    if(node.getCodeblock() != null) {
      node.getCodeblock().apply(this)
      
      // Jump to the end of the if/else expression
      mv.visitJumpInsn(GOTO, endLabel)
    }
    
    // The else block begins now
    if(node.getElse() != null) {
      visitLabel(elseLabel)
      node.getElse().apply(this)
    }
    visitLabel(endLabel)
  }
  
  private def putBooleanOnStack(integerLoadInstruction : Int) {
    val mv = methodVisitors.head

    // Up-cast to a boolean
    // we don't like leave primitive types on the stack
    // This is inefficient but it simplifies implementation
    mv.visitInsn(integerLoadInstruction)
    mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;")
  }
  
  override def outAFalseExpr(node : AFalseExpr) = putBooleanOnStack(ICONST_0)
  
  override def outATrueExpr(node : ATrueExpr) = putBooleanOnStack(ICONST_1)
  
  override def caseAForloop(node : AForloop) {
    val mv = methodVisitors.head
    inAForloop(node)
    if (node.getType() != null) {
      node.getType().apply(this)
    }
    if (node.getIdentifier() != null) {
      node.getIdentifier().apply(this)
    }
    
//    setLineNumber(mv, startOrContinueLabel(node), lineNumberProvider.getLineNumber(node))
    if (node.getExpr() != null) {
      node.getExpr().apply(this)
    }

    // The iterable should be on the top of the stack
    mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "iterator", "()Ljava/util/Iterator;")

    val l2 = new Label()
    setLineNumber(mv, l2, lineNumberProvider.getLineNumber(node))
    mv.visitJumpInsn(GOTO, l2)

    val l3 = new Label()
    mv.visitLabel(l3)
    setLineNumber(mv, l3, lineNumberProvider.getLineNumber(node))

    mv.visitInsn(DUP) // iterator, iterator
    mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;") // object, iterator

    // TODO Check the type of the expression against what we get

    val l4 = new Label()
    mv.visitLabel(l4)
    
    // Push the value on to the scope
    addToScopeFromStack(node.getIdentifier().getText()) // iterator
    
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
  
  def outputClass(cw : ClassWriter, file : File) {
    val bytes = cw.toByteArray()
    
    val stream = new FileOutputStream(file)
    try {
      stream.write(bytes)
    } finally {
      stream.close()
    }
  }
  
  def outputClass(cw : ClassWriter, name : String) { outputClass(cw, new File("temp/" + name + ".class")) } 
  
  override def outAView(node : AView) {
    if (node.getStmt().size() > 0) {
      trailingLabels.pop
      
      // Return the list of nodes
      val mv = methodVisitors.head
      val l0 = labels.head
      
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
      
      classWriters.head.visitEnd()
      
      scopes.pop()
      
      outputClass(classWriter, new File(outputFileName))
    }
  }
  
  override def outAPropertyrefExpr(node : APropertyrefExpr) {
    val path = IdentifierConverter.identifierListToString(node.getIdentifier())

    val l0 = new Label()
    val mv = methodVisitors.head
    mv.visitLabel(l0)
    
    NEW(classOf[PropertyReference[_]], classOf[String], classOf[Object]) {
      mv.visitLdcInsn(path)
      
      // Get the value of the property
      getFromScope("$controller")
      CHECKCAST(controllers.head)
      evaluateProperty(node.getIdentifier(), controllers.head)
    }
    
    // Property reference is left on the stack
  }
  
  override def outAPropertyExpr(node : APropertyExpr) {
    val mv = methodVisitors.head
    
    // Get the value from the scope
    val l1 = new Label()
    mv.visitLabel(l1)
    
    val name = IdentifierConverter.identifierListToString(node.getIdentifier)

    // Ignore dynamic macro assignments, they're handled in the assignment handler
    if (!macros.contains(name)) {
      getFromScope(node.getIdentifier.head.getText)
        
      if (node.getIdentifier().size() > 1) {
        // Cast as appropriate
        val ownerType = typeResolver.getType(node.getIdentifier.head, typeNameResolver)
        CHECKCAST(ownerType.asInstanceOf[SimpleType].getClazz)
        
        evaluateProperty(node.getIdentifier.tail, ownerType.asInstanceOf[SimpleType].getClazz)
      }
    }
    
    // Leave the property on the stack for the next instruction to pick up
  }
  
  override def outAControllerPropExpr(node : AControllerPropExpr) {
    val mv = methodVisitors.head
    
    // Get the value from the scope
    val l1 = new Label()
    mv.visitLabel(l1)
    getFromScope("$controller")
    CHECKCAST(controllers.head)
    evaluateProperty(node.getIdentifier(), controllers.head)
  }
  
  def evaluateProperty(identifiers : Iterable[TIdentifier], ownerType : Class[_]) {
    val mv = methodVisitors.head
    
    // The controller will be on the stack
    val name = identifiers.head.getText
    
    /// Determine the method return type
    var methodName = "get" + capitalise(name)
    var returnType : String = null
    var returnClass : Class[_] = null
    var isInterface = false

    returnClass = ownerType.getMethod(methodName).getReturnType()
    returnType = Type.getInternalName(returnClass)
    isInterface = ownerType.isInterface
    
    val invokeType = if (isInterface) INVOKEINTERFACE else INVOKEVIRTUAL
    mv.visitMethodInsn(invokeType, Type.getInternalName(ownerType), methodName, "()L" + returnType + ";")
    
    // Recurse if needed
    if (identifiers.size > 1) {
      evaluateProperty(identifiers.tail, returnClass)
    }
  }
  
  def capitalise(s : String ) = {
    s.substring(0, 1).toUpperCase() + s.substring(1)
  }
  
  override def inAView(node : AView) {
    // Output a view class only if the view contains something that isn't a macro definition
    if (node.getStmt().size() > 0) {
      trailingLabels.push(None)
      
      val classWriter = classWriters.head
      classWriter.visit(V1_6, ACC_PUBLIC + ACC_SUPER, viewname, null, "uk/co/colinhowe/glimpse/View", Array[String]())
      // TODO Check up on relative paths
      classWriter.visitSource(sourcename, null)
      
      // TODO Move this next to initialisation if possible
      val fv = classWriter.visitField(
          ACC_PRIVATE, "globalScope", Type.getDescriptor(classOf[Scope]), null, null)
      fv.visitEnd()
  
      // Create a scope for the view
      val scope = new Scope(null, null) // TODO Set the owner
      scopes.push(scope)
      
      // Constructor
      {
        val mv = classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
        methodVisitors.push(mv)
        mv.visitCode()
  
        // Initialise super class
        val l0 = new Label()
        mv.visitLabel(l0)
        mv.visitVarInsn(ALOAD, 0)
        mv.visitMethodInsn(INVOKESPECIAL, "uk/co/colinhowe/glimpse/View", "<init>", "()V")
    
        // Initialise the scope
        mv.visitVarInsn(ALOAD, 0)
        NEW(classOf[Scope], classOf[Scope], classOf[Object]) {
          mv.visitInsn(ACONST_NULL)
          mv.visitLdcInsn(getOwner(node))
        }
        mv.visitFieldInsn(PUTFIELD, viewname, "globalScope", Type.getDescriptor(classOf[Scope]))
        
        val l2 = new Label()
        mv.visitLabel(l2)
        mv.visitInsn(RETURN)
    
        val l3 = new Label()
        mv.visitLabel(l3)
        mv.visitLocalVariable("this", "L" + viewname + ";", null, l0, l3, 0)
        mv.visitMaxs(0, 0)
        mv.visitEnd()
        methodVisitors.pop
      } 
      
      // Create the view method
      {
        // TODO Make this use the classes... currently mess!
        val mv = classWriter.visitMethod(
            ACC_PUBLIC, "view", "(Ljava/lang/Object;)Ljava/util/List;", "(Ljava/lang/Object;)Ljava/util/List<Luk/co/colinhowe/glimpse/Node;>;", null)
        methodVisitors.push(mv)
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
        
        NEW(classOf[java.util.LinkedList[_]]) { }
        mv.visitVarInsn(ASTORE, 2);
        
        // TODO Stuff goes here
      }
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
    val mv = methodVisitors.head
    mv.visitLdcInsn(node.getText())
  }
  
  override def outAController(node : AController) {
    // The controller will be in register 3... put it on the environment
    val mv = methodVisitors.head
    addToScope("$controller") {
      mv.visitVarInsn(ALOAD, 3)
    }
    
    // Load the controller class
    // TODO Worry about this in macros!
    if (node.parent.isInstanceOf[AView]) {
      controllers.push(typeResolver.getType(node.getType, typeNameResolver).asInstanceOf[SimpleType].getClazz)
    }
  }

  private def defaultConstructor(cw : ClassWriter, className : String, parentClass : String = "java/lang/Object") {
    val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    mv.visitCode()

    // Initialise super class
    val l0 = new Label()
    mv.visitLabel(l0)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKESPECIAL, parentClass, "<init>", "()V")
    mv.visitInsn(RETURN)

    val l3 = new Label()
    mv.visitLabel(l3)
    mv.visitLocalVariable("this", "L" + className + ";", null, l0, l3, 0)
    mv.visitMaxs(0, 0)
    mv.visitEnd()
  }
  
  override def caseAArgDefn(node : AArgDefn) {
    // Do nothing, we don't want these processed
  }
  
  override def inAMacroDefn(node : AMacroDefn) {
    inMacro = true
    
    // Create a scope for this macro
    val scope = new Scope(if(scopes.isEmpty) null else scopes.head, null) // TODO Set the owner
    scopes.push(scope)
    
    // Create a new top level class
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    classWriters.push(cw)
    
    // Create the signature for the macro
    val args = scala.collection.mutable.Set[(String, String)]()
    for (parg <- node.getArgDefn()) {
      val arg = parg.asInstanceOf[AArgDefn]
      val argName = arg.getIdentifier().toString()
      args.add((argName, arg.getType().toString()))
    }

    val macroDefinition : MacroDefinition = node
    
    // Get the name of the macro
    val macroName = node.getName().getText()
    if (macroDefinition.isDynamic) {
      dynamicMacros.add(macroName)
    } else {
      macros.add(macroName)
    }

    val className = macroDefinition.className
    val (parentClass, interfaces) = if (macroDefinition.isAbstract) {
      ("uk/co/colinhowe/glimpse/infrastructure/RuntimeTypedMacro", Array[String]())
    } else if (macroDefinition.isDynamic) {
      ("uk/co/colinhowe/glimpse/infrastructure/DynamicMacro", Array[String]())
    } else {
      ("java/lang/Object", Array[String]("uk/co/colinhowe/glimpse/Macro"))
    }
    cw.visit(V1_6, ACC_SUPER + ACC_PUBLIC, className, null, parentClass, interfaces)
    cw.visitSource(sourcename, null)

    // Instance field for the macro
    val fv = cw.visitField(ACC_PRIVATE + ACC_STATIC, "instance", "Luk/co/colinhowe/glimpse/Macro;", null, null)
    fv.visitEnd()
    
    // Static constructor
    {
      val mv = cw.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null)
      methodVisitors.push(mv)
      mv.visitCode()

      // Initialise the instance
      NEW(className) { }
      mv.visitFieldInsn(PUTSTATIC, className, "instance", "Luk/co/colinhowe/glimpse/Macro;")
      
      mv.visitInsn(RETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
      methodVisitors.pop
    } 
    
    // getInstance method
    {
      val mv = cw.visitMethod(ACC_STATIC | ACC_PUBLIC, "getInstance", "()Luk/co/colinhowe/glimpse/Macro;", null, null)
      mv.visitCode()

      mv.visitFieldInsn(GETSTATIC, className, "instance", "Luk/co/colinhowe/glimpse/Macro;")
      mv.visitInsn(ARETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd();
    } 
    
    // getMacroName method
    if (macroDefinition.isAbstract) {
      val mv = cw.visitMethod(ACC_PUBLIC, "getMacroName", "()Ljava/lang/String;", null, null)
      mv.visitCode()

      mv.visitLdcInsn(macroDefinition.name)
      mv.visitInsn(ARETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd();
    } 
    
    // getRuntimeTypedArgumentName method
    if (macroDefinition.isAbstract) {
      // Find the runtime typed argument
      val argDefn = macroDefinition.arguments.values.find(_.isRuntimeTyped).get
      
      val mv = cw.visitMethod(ACC_PUBLIC, "getRuntimeTypedArgumentName", "()Ljava/lang/String;", null, null)
      mv.visitCode()

      mv.visitLdcInsn(argDefn.name)
      mv.visitInsn(ARETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd();
    } 
    
    defaultConstructor(cw, className, parentClass)
    
    // Invoke method
    if (!macroDefinition.isDynamic && !macroDefinition.isAbstract) {
      
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
      NEW(classOf[Scope], classOf[Scope], classOf[Object]) {
        mv.visitVarInsn(ALOAD, 1)
        mv.visitLdcInsn(getOwner(node))
      }
      mv.visitVarInsn(ASTORE, 4)
      
      // Put the controller on if needed
      if (macroDefinition.controller != null) {
        mv.visitVarInsn(ALOAD, 4) // scope
        mv.visitLdcInsn("$controller") // name, scope
        getFromScope("$controller") // value, name, scope
        mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V")
      }
      
      // Put defaults on as appropriate
      for (parg <- node.getArgDefn()) {
        val arg = parg.asInstanceOf[AArgDefn]
        
        if (arg.getDefault != null) {
          // Load the default on to the stack and put it on the scope
          mv.visitVarInsn(ALOAD, 4) // scope
          mv.visitLdcInsn(arg.getIdentifier.getText) // name, scope
          arg.getDefault.apply(this) // value, name, scope
          mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V")
        }
      }
      
      // Go over each argument and retrieve it from the argument map if possible
      for ((name, arg) <- macroDefinition.arguments) {
        mv.visitVarInsn(ALOAD, 2) // argmap
        mv.visitLdcInsn(name) // name, argmap
        mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Map", "containsKey", "(Ljava/lang/Object;)Z")

        // contained, name
        val endAdd = new Label()
        mv.visitJumpInsn(IFEQ, endAdd)

        // Put the content onto the scope
        mv.visitVarInsn(ALOAD, 4) // scope
        mv.visitVarInsn(ALOAD, 2) // argmap , scope
        mv.visitLdcInsn(name) // name, argmap, scope
        mv.visitInsn(DUP_X1) // name, argmap, name, scope
        mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Map", "get", "(Ljava/lang/Object;)Ljava/lang/Object;")
        // value, name, scope
        mv.visitLdcInsn(0)
        mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;Z)V")
        mv.visitLabel(endAdd)
      }
      
      // Put cascade arguments on the caller's scope as needed
      for ((name, arg) <- macroDefinition.arguments) {
        if (arg.cascade) {
          mv.visitVarInsn(ALOAD, 1) // scope
          mv.visitLdcInsn("$" + name) // name, scope

          mv.visitVarInsn(ALOAD, 4) // scope
          mv.visitLdcInsn(name) // name, scope
          mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;")

          mv.visitLdcInsn(1) // true, value, name, scope
          mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;Z)V")
        }
      }
      
      // If the content is a generator then we must put the scope into a suitably named variable
      if (macroDefinition.valueType == SimpleType(classOf[Generator])) {
        mv.visitVarInsn(ALOAD, 4) // scope
        mv.visitLdcInsn("$ownerscope") // name, scope
        mv.visitVarInsn(ALOAD, 1) // parent scope, name, scope
        mv.visitLdcInsn(1) // true, parent scope, name, scope
        mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;Z)V")
      }
      
      // Put the content onto the scope
      if (node.getWithDefn != null) {
        mv.visitVarInsn(ALOAD, 4) // scope
        val valueName = node.getWithDefn.asInstanceOf[AWithDefn].getContentName().getText()
        mv.visitLdcInsn(valueName) // name, scope
        mv.visitVarInsn(ALOAD, 3) // value, name, scope
        mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V")
      }
      
      val l6 = new Label()
      mv.visitLabel(l6)
      mv.visitLocalVariable("this", "Lcheese/HelloWorld;", null, l0, l6, 0)
      mv.visitLocalVariable("callerScope", "Luk/co/colinhowe/glimpse/infrastructure/Scope;", null, l0, l6, 1)
      mv.visitLocalVariable("_args", "Ljava/util/Map;", "Ljava/util/Map<Ljava/lang/String;Ljava/lang/Object;>;", l0, l6, 2)
      mv.visitLocalVariable("value", "Ljava/lang/String;", null, l0, l6, 3)
      mv.visitLocalVariable("scope", "Luk/co/colinhowe/glimpse/infrastructure/Scope;", null, l0, l6, 4)
    }
    
    if (node.getController != null) {
      controllers.push(typeResolver.getType(node.getController.asInstanceOf[AController].getType, typeNameResolver).asInstanceOf[SimpleType].getClazz)
    } else {
      controllers.push(null)
    }
  }
  
  
  override def outAMacroDefn(node : AMacroDefn) {
    val macroDefinition : MacroDefinition = node
    val className = macroDefinition.className
    inMacro = false

    scopes.pop()
    controllers.pop
    
    if (!macroDefinition.isDynamic && !macroDefinition.isAbstract) {
      val mv = methodVisitors.pop()

      // The generator will be on the stack
      val generatorId = generatorIds.get(node.getGenerator())
      val generatorIdentifier = viewname + "$$generator" + generatorId

      // generator
      mv.visitVarInsn(ALOAD, 4) // scope, generator
      mv.visitMethodInsn(INVOKEINTERFACE, "uk/co/colinhowe/glimpse/Generator", "view", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;)Ljava/util/List;")
      // nodes
  
      mv.visitInsn(ARETURN)
  
      mv.visitMaxs(4, 7)
      mv.visitEnd()
    }
    
    val cw = classWriters.pop()
    cw.visitEnd()

    outputClass(cw, className)
  }
  
  private def beginScope(
      owner : String, 
      store : Boolean = true, 
      putParentScopeOnStack : MethodVisitor => Unit = (_.visitVarInsn(ALOAD, 1))) {
    // TODO Limit starting scopes to only those things that _need_ new scopes
    // If no new variables are created then a new scope isn't needed
    val mv = methodVisitors.head
    NEW(classOf[Scope], classOf[Scope], classOf[Object]) {
      putParentScopeOnStack(mv)
      if (owner != null) {
        mv.visitLdcInsn(owner)
      } else {
        // Get the owner from the parent
        mv.visitInsn(DUP)
        mv.visitMethodInsn(INVOKEVIRTUAL, Type.getInternalName(classOf[Scope]), "owner", "()Ljava/lang/Object;"); 
      }
    }
    if (store) {
      mv.visitInsn(DUP)
      mv.visitVarInsn(ASTORE, 1)
    }
    
    val scope = new Scope(scopes.head, null) // TODO Set the owner
    scopes.push(scope)
  }
  
  private def endScope {
    scopes.pop
  }
  
  private def getOwner(currentNode : Node) : String = {
    currentNode match {
      case defn : AMacroDefn => defn.getName.getText
      case _ : AView => "view"
      case _ => getOwner(currentNode.parent)
    }
  }
  
  override def outAIncludeStmt(node : AIncludeStmt) {
    val mv = methodVisitors.head
    
    val include = node.getIncludeA().asInstanceOf[AIncludeA]

    inMacro = false
    beginScope(null, false, mv => {
      getFromScope("$ownerscope")
      CHECKCAST(classOf[Scope])
    })
    inMacro = true
    
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
    getFromScope(generatorArgumentName)
    
    // The generator will be on the stack
    CHECKCAST(classOf[Generator])

    // invoke the generator
    mv.visitInsn(SWAP)
    mv.visitMethodInsn(INVOKEINTERFACE, "uk/co/colinhowe/glimpse/Generator", "view", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;)Ljava/util/List;")

    addAllNodesFromStack
    
    endScope
  }

  override def inAGenerator(node : AGenerator) {
    trailingLabels.push(None)
    
    // Create a scope for this generator
    val scope = new Scope(scopes.head, null) // TODO Set the owner
    scopes.push(scope)
    
    val outerClassWriter = classWriters.head
    
    // Get the ID for the generator
    val id = generatorCount
    generatorCount += 1
    val generatorName = "$generator" + id

    generatorNames.push(generatorName)
    
    outerClassWriter.visitInnerClass(viewname + "$" + generatorName, viewname, generatorName, ACC_PRIVATE + ACC_STATIC)

    val innerClassWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    classWriters.push(innerClassWriter)

    innerClassWriter.visit(V1_6, ACC_SUPER, viewname + "$" + generatorName, null, "java/lang/Object", Array[String]("uk/co/colinhowe/glimpse/Generator"))
    innerClassWriter.visitSource(sourcename, null)
    val fullClassName = viewname + "$" + generatorName
    innerClassWriter.visitInnerClass(fullClassName, viewname, generatorName, ACC_PRIVATE + ACC_STATIC)
 
    generatorIds.put(node, id)

    // Instance field for the generator
    val fv = innerClassWriter.visitField(ACC_PRIVATE + ACC_STATIC, "instance", "Luk/co/colinhowe/glimpse/Generator;", null, null)
    fv.visitEnd()
    
    // Static constructor
    {
      val mv = innerClassWriter.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null)
      mv.visitCode()
      methodVisitors.push(mv)

      // Initialise the instance
      NEW(fullClassName) { }
      mv.visitFieldInsn(PUTSTATIC, fullClassName, "instance", "Luk/co/colinhowe/glimpse/Generator;")
      
      mv.visitInsn(RETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd(); 
      methodVisitors.pop
    }
    
    // getInstance method
    {
      val mv = innerClassWriter.visitMethod(ACC_STATIC | ACC_PUBLIC, "getInstance", "()Luk/co/colinhowe/glimpse/Generator;", null, null)
      mv.visitCode()

      val l1 = new Label()
      mv.visitLabel(l1)
      mv.visitFieldInsn(GETSTATIC, fullClassName, "instance", "Luk/co/colinhowe/glimpse/Generator;")
      mv.visitInsn(ARETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd(); 
    } 
    
    defaultConstructor(innerClassWriter, viewname + "$" + generatorName)
    
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
      methodVisitors.push(mv)
      mv.visitCode()
      
      val l1 = new Label()
      mv.visitLabel(l1)
      NEW(classOf[java.util.LinkedList[_]]) { }
      mv.visitVarInsn(ASTORE, 2)
      
      // TODO Stuff goes here
      labels.push(l1)

      beginScope(getOwner(node))
    }
  }

  override def outAGenerator(node : AGenerator) {
    scopes.pop()
    trailingLabels.pop

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
    
    // Get the instance of the generator
    mv = methodVisitors.head
    mv.visitMethodInsn(INVOKESTATIC, generatorIdentifier, "getInstance", "()Luk/co/colinhowe/glimpse/Generator;") // macro, value, args
  }

  override def outAInvertExpr(node : AInvertExpr) {
    // Assume that a Boolean is on the stack
    val mv = methodVisitors.head
    CHECKCAST(classOf[java.lang.Boolean])
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z")
    mv.visitLdcInsn(1)
    mv.visitInsn(IXOR) // 1 ^ x is equivalent to !x
    mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;")
  }
  
  override def outAVarDefn(node : AVarDefn) {
    val mv = methodVisitors.head

    val varname = node.getIdentifier().getText()
    if (node.getExpr() != null) {
      val l0 = startOrContinueLabel(node)
      setLineNumber(mv, l0, lineNumberProvider.getLineNumber(node))
      mv.visitLabel(l0)
      addToScopeFromStack(varname)
    }
    
    // Put this variable and the type of it on to the scope
    scopes.head.add(varname, typeResolver.getType(node, typeNameResolver))
  }

  override def outAIncrementStmt(node : AIncrementStmt) {
    // Get the value from the scope
    val mv = methodVisitors.head
//    setLineNumber(mv, startOrContinueLabel(node), lineNumberProvider.getLineNumber(node))
    mv.visitVarInsn(ALOAD, 1) // scope
    mv.visitLdcInsn(node.getIdentifier().getText()) // name, scope
    mv.visitTypeInsn(Opcodes.NEW, "java/lang/Integer") // Integer, name, scope
    mv.visitInsn(DUP) // Integer, Integer, name, scope
    getFromScope(node.getIdentifier().getText()) // value, Integer, Integer, name, scope
    CHECKCAST(classOf[java.lang.Integer])
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I")
    mv.visitInsn(ICONST_1) // 1, value, Integer, Integer, name, scope
    mv.visitInsn(IADD) // value+1, Integer, Integer, name, scope
    mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V") // Integer, name, scope
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "replace", "(Ljava/lang/String;Ljava/lang/Object;)V")
  }
  
  private def outDynamicMacroAssignment(node : AAssignmentStmt) {
    val mv = methodVisitors.head
    val dynamicMacroName = node.getIdentifier().getText()
    val expr = node.getExpr().asInstanceOf[APropertyExpr]
    val sourceMacroName = IdentifierConverter.identifierListToString(expr.getIdentifier)

    // Get the dynamic macro definition
    val dynamicMacroDefinition = callResolver.getMacrosWithName(dynamicMacroName).iterator.next
    
    // Get the source macro definition
    val sourceMacroDefinitions = callResolver.getMacrosWithName(sourceMacroName)
    
    // Search for an exact match
    val sourceMacroDefinition = sourceMacroDefinitions.find(defn =>
        defn.valueType == dynamicMacroDefinition.valueType &&
        defn.arguments == dynamicMacroDefinition.arguments)
    val sourceClassName = sourceMacroDefinition.get.className
    
    mv.visitMethodInsn(INVOKESTATIC, sourceClassName, "getInstance", "()Luk/co/colinhowe/glimpse/Macro;") // target
    
    mv.visitMethodInsn(INVOKESTATIC, dynamicMacroName, "getInstance", "()Luk/co/colinhowe/glimpse/Macro;") // target
    CHECKCAST(classOf[DynamicMacro])
    mv.visitInsn(SWAP)
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/DynamicMacro", "setToInvoke", "(Luk/co/colinhowe/glimpse/Macro;)V")
  }
  
  override def outAAssignmentStmt(node : AAssignmentStmt) {
    val mv = methodVisitors.head
    
    // This could be a dynamic macro assignment
    val destinationVariable = node.getIdentifier().getText()
    
    startOrContinueLabel(node)
    if (dynamicMacros.contains(destinationVariable)) {
      outDynamicMacroAssignment(node)
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
    val mv = methodVisitors.head
    val i = Integer.valueOf(node.getNumber().getText())
    mv.visitLdcInsn(i)

    // Up-cast to an Integer
    // we don't like leaving primitive types on the stack
    // This is inefficient but it simplifies implementation
    mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;")
  }

  override def outAMacroStmt(node : AMacroStmt) {
    val mv = methodVisitors.head
    val call = resolvedCallsProvider.get(node) 

    if (node.getExpr == null) {
      // Load null on to the stack as the value when there is no expression 
      mv.visitInsn(ACONST_NULL)
    }

    // The arguments will be on the stack already.
    // The stack will look like:
    //   macro value, arg, arg, ...
    
    // Put all the arguments into a map
    NEW(classOf[java.util.HashMap[_,_]]) { }

    // args, macro value, arg
    CHECKCAST(classOf[java.util.Map[_,_]])

    val args = node.getArguments()
    val argTypes = MMap[String, GType]()
    
    for (pargument <- args) {
      val argument = pargument.asInstanceOf[AArgument]

      // Shuffle the stack around so the macro value and args are at the bottom

      // args, macro value, arg
      mv.visitInsn(SWAP) // macro value, args, arg
      mv.visitInsn(DUP2_X1) // macro value, args, arg, macro value, args
      mv.visitInsn(POP) // args, arg, macro value, args
      mv.visitInsn(SWAP) // arg, args, macro value, args
      
      // Put the variable name on
      val argName = argument.getIdentifier().getText()
      mv.visitLdcInsn(argName)
      // name, arg, args, macro value, args
      mv.visitInsn(SWAP) // arg, name, args, macro value, args
      
      // Put the variable on the arguments
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Map", "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;")
      // object, macro value, args
      mv.visitInsn(POP) // macro value, args
      mv.visitInsn(SWAP) // args, macro value
      
      argTypes(argName) = typeResolver.getType(argument.getExpr, typeNameResolver)
    }
    
    // Fill in any missing arguments from cascades
    for ((name, source) <- call.argumentSources) {
      source match {
        case Cascade =>
          mv.visitInsn(DUP) // args
          mv.visitLdcInsn(name) // name, args
          getFromScope("$" + name) // value, name, args
          
          // Put the variable on the arguments
          mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Map", "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;")
          // object
          mv.visitInsn(POP) // <empty>
        case _ =>
      }
    }
    
    val macroName = node.getIdentifier().getText()
    
    // Stack: args, value (string)
    mv.visitInsn(SWAP) // value, args
    
    // Determine what macro to invoke
    mv.visitMethodInsn(INVOKESTATIC, call.macro.className, "getInstance", "()Luk/co/colinhowe/glimpse/Macro;") // macro, value, args
    
    // macro, value, args
    mv.visitInsn(DUP_X2) // macro, value, args, macro
    mv.visitInsn(POP) // value, args, macro

    mv.visitVarInsn(ALOAD, 1) // scope, value, args, macro
    mv.visitInsn(DUP_X2) // scope, value, args, scope, macro
    mv.visitInsn(POP) // value, args, scope, macro

    CHECKCAST(classOf[java.lang.Object]) // value, args, scope, macro
    mv.visitMethodInsn(INVOKEINTERFACE, "uk/co/colinhowe/glimpse/Macro", "invoke", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Ljava/util/Map;Ljava/lang/Object;)Ljava/util/List;")
    
    addAllNodesFromStack
  }
  
  
  /**
   * When we come in to this method we expect the following stack:
   *   value
   *   property value -- Property pairs
   *   property name  -|
   */
  override def caseANodeCreate(node : ANodeCreate) {
    val id = node.getId().getText()
    val mv = methodVisitors.head
    
    // Start creating the node
    // Load up the node list ready for adding the node to
    // Create the node on the stack ready for setting properties on it
    val l1 = startLabel(node)
    
    NEW(classOf[uk.co.colinhowe.glimpse.Node], classOf[java.util.List[_]], classOf[String], classOf[Object]) {
      node.getExpr match {
        case null => 
          mv.visitInsn(ACONST_NULL)
          mv.visitLdcInsn(id)
          mv.visitInsn(ACONST_NULL)
        
        case expr : AGeneratorExpr => 
          expr.apply(this)

          val generatorIdentifier = viewname + "$$generator" + generatorIds.get(expr.getGenerator())
          // Stack: generator, node, node
          mv.visitVarInsn(ALOAD, 1) // scope, generator, node, node
          mv.visitMethodInsn(INVOKEINTERFACE, "uk/co/colinhowe/glimpse/Generator", "view", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;)Ljava/util/List;")
          // nodes, node, node
          mv.visitLdcInsn(id);  // id, nodes, node, node
          mv.visitInsn(ACONST_NULL) // null, id, nodes, node, node
        
        case expr => 
          // TODO Store the node off in a local variable, n
          // Then visitLocalVariable in this method.
          // This is fine as we can limit scope of the variable using labels and all is good
          
          mv.visitInsn(ACONST_NULL) // null, node, node
          mv.visitLdcInsn(id) // id, null, node, node
          expr.apply(this) // value, id, null, node, node
      }
    }
      
    for (aargument <- node.getArguments) {
      val argument = aargument.asInstanceOf[AArgument]

      mv.visitInsn(DUP) // node, node
      mv.visitLdcInsn(argument.getIdentifier().getText()) // name, node, node
      argument.getExpr.apply(this) // value, name, node, node
      mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/Node", "setAttribute", "(Ljava/lang/String;Ljava/lang/Object;)V")
    }
    
    addToNodeListFromStack
  }
}