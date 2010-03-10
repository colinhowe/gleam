package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.IdentifierNotFoundException
import uk.co.colinhowe.glimpse.MethodNotFoundError
import uk.co.colinhowe.glimpse.IncompatibleControllerError

import java.lang.reflect.Method

import scala.collection.JavaConversions
import scala.collection.JavaConversions._
import scala.collection.mutable.{ Map => MMap, Buffer, Stack => MStack }
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl
import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.DynamicMacroMismatchError
import uk.co.colinhowe.glimpse.Generator
import uk.co.colinhowe.glimpse.IdentifierNotFoundError
import uk.co.colinhowe.glimpse.MacroNotFoundError
import uk.co.colinhowe.glimpse.IdentifierNotFoundException
import uk.co.colinhowe.glimpse.MapUtil
import uk.co.colinhowe.glimpse.TypeCheckError
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.compiler.node._
import uk.co.colinhowe.glimpse.compiler.typing.CompoundType
import uk.co.colinhowe.glimpse.compiler.typing.GenericType
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.infrastructure.Scope
import uk.co.colinhowe.glimpse.MapUtil
import uk.co.colinhowe.glimpse.PropertyReference
import uk.co.colinhowe.glimpse.compiler.IdentifierConverter._
 
class TypeChecker(
    val lineNumberProvider : LineNumberProvider,
    val macroProvider : MacroDefinitionProvider,
    val typeResolver : TypeResolver,
    val typeNameResolver : TypeNameResolver,
    val callResolver : CallResolver
  ) extends DepthFirstAdapter {
  
  val resolvedCallsProvider = new ResolvedCallsProvider
  val owners = new MStack[String]()
  owners.push("view")
  
  private val methodResolver = new MethodResolver(typeResolver, typeNameResolver)
  private val cascadeIdentifier = new CascadeIdentifier(
      typeResolver, typeNameResolver, resolvedCallsProvider)
  
  /*
   * TODO I think this could all be made a lot tidier
   * 
   * op(!, expects=boolean, returns=boolean)
   * op(++, expects=integer, returns=integer)
   * op(propertyReference, returns=typeOf(property))
   */
  val errors = scala.collection.mutable.Buffer[CompilationError]()
  val genericsInScope = scala.collection.mutable.Map[String,Type]()
  
  var scope : Scope = new Scope(null, "view")
  var controllers = new MStack[Class[_]]()
  controllers.push(null)
    
  def getExpressionType(expr : PExpr) : Type = {
    expr match {
      case _ : AStringExpr => new SimpleType(classOf[java.lang.String])
      case _ : AConstantExpr => new SimpleType(classOf[java.lang.Integer])
      case _ : AFalseExpr | 
           _ : ATrueExpr 
             => new SimpleType(classOf[java.lang.Boolean])
      case _ : AInvertExpr => new SimpleType(classOf[java.lang.Boolean])
      case _ : APropertyExpr => typeResolver.getType(expr, typeNameResolver)
      case _ : AGeneratorExpr => new SimpleType(classOf[Generator])
      case expr : AControllerPropExpr => typeResolver.getType(expr, typeNameResolver)
      case expr : AControllerMethodExpr => new SimpleType(
          methodResolver.getMatchingMethod(controllers.head, expr).get.getReturnType
        )
      case _ : AErrorExpr => new SimpleType(classOf[java.lang.Object])
      case _ => throw new IllegalArgumentException("Cannot handle expression[" + expr + ":" + expr.getClass + "]")
    }
  }

  /**
   * Determine if the two types are compatible (the same, easily castable or a child type).
   * 
   * @param destinationType
   * @param sourceType
   * @return
   */
  def areTypesCompatible(destinationType : Type, sourceType : Type) : Boolean = {
    if (destinationType.isInstanceOf[MacroDefinition] && sourceType.isInstanceOf[MacroDefinition]) {
      return (destinationType.asInstanceOf[MacroDefinition]).areCompatible(sourceType.asInstanceOf[MacroDefinition])
    }
    
    if (destinationType.isInstanceOf[CompoundType] && sourceType.isInstanceOf[CompoundType]) {
      val cdt = destinationType.asInstanceOf[CompoundType]
      val sdt = sourceType.asInstanceOf[CompoundType]
      return sdt.clazz.isAssignableFrom(cdt.clazz)
    }
    
    return sourceType.canBeAssignedTo(destinationType)
  }
  
  override def outAIncrementStmt(node : AIncrementStmt) {
    // Check that the variable is indeed an integer
    try {
      val t = scope.get(node.getIdentifier().getText()).asInstanceOf[Type]
      if (!t.equals(new SimpleType(classOf[java.lang.Integer]))) {
        errors += new TypeCheckError(lineNumberProvider.getLineNumber(node).get, new SimpleType(classOf[java.lang.Integer]), t)
      }
    } catch {
      case _ : IdentifierNotFoundException =>
        errors += new IdentifierNotFoundError(lineNumberProvider.getLineNumber(node).get, node.getIdentifier().getText())
    }
  }
  
  def casePStmt(node : PStmt) {
    try {
      node.apply(this)
    } catch {
      case e : IdentifierNotFoundException =>
        node.replaceBy(new AErrorNode)
        errors += new IdentifierNotFoundError(lineNumberProvider.getLineNumber(node).get, e.identifier)
    }
  }
  
  override def outAControllerMethodExpr(node : AControllerMethodExpr) {
    // Check that the method matches something
    val methodOption = methodResolver.getMatchingMethod(controllers.head, node)
    
    methodOption match {
      case None =>
        val argTypes = node.getExpr.map { expr =>
          typeResolver.getType(expr, typeNameResolver)
        }
        errors += MethodNotFoundError(
          line = lineNumberProvider.getLineNumber(node).get,
          identifier = node.getIdentifier.getText,
          arguments = argTypes.toList
        )
        
        // Remove this node
        node.replaceBy(new AErrorExpr)
      case Some(method) =>
        typeResolver.addType(node, new SimpleType(method.getReturnType))
    }
  }
  
  override def outAVarDefn(node: AVarDefn) {
    // Get the RHS type
    val rhsType = getExpressionType(node.getExpr())
    val varName = node.getIdentifier().getText()
    scope.add(varName, rhsType)
  }
  
  implicit def toImmutableMap[K, V](mutable : scala.collection.Map[K, V]) = scala.collection.immutable.Map[K, V]() ++ mutable
  
  
  override def inAForloop(node : AForloop) {
    scope = new Scope(scope, owners.head)
    scope.add(node.getIdentifier().getText(), typeResolver.getType(node.getType(), typeNameResolver, genericsInScope))
  }
  
  override def outAForloop(node : AForloop) {
    scope = scope.parentScope
  }
  
  override def inAMacroDefn(node : AMacroDefn) {
    owners.push(node.getName.getText)
    scope = new Scope(scope, owners.head)
    
    for (pdefn <- node.getGenericDefn()) {
      val defn = pdefn.asInstanceOf[AGenericDefn]
      val nodeType = typeResolver.getType(defn, typeNameResolver, genericsInScope)

      // Put this generic in scope
      genericsInScope.put(defn.getIdentifier().getText(), nodeType)
    }
    
    for (parg <- node.getArgDefn()) {
      val arg = parg.asInstanceOf[AArgDefn]
      scope.add(arg.getIdentifier().getText(), typeResolver.getType(arg.getType(), typeNameResolver, genericsInScope))
    }
    
    if (node.getWithDefn != null) {
      scope.add(node.getWithDefn.asInstanceOf[AWithDefn].getContentName().getText(), typeResolver.getType(node.getWithDefn.asInstanceOf[AWithDefn].getContentType, typeNameResolver, genericsInScope))
    }
    
    if (node.getController != null) {
      controllers.push(typeResolver.getType(node.getController.asInstanceOf[AController].getType, typeNameResolver).asInstanceOf[SimpleType].getClazz)
    } else {
      controllers.push(null)
    }
  }
  
  override def outAMacroDefn(node : AMacroDefn) {
    owners.pop
    controllers.pop
    scope = scope.parentScope

    // Clear any generics in scope
    genericsInScope.clear()
  }
  
  override def caseAMacroStmt(node : AMacroStmt) {
    inAMacroStmt(node)
    if(node.getIdentifier() != null) {
      node.getIdentifier().apply(this)
    }
    node.getArguments.foreach(_.apply(this))

    // Generator expressions must be processed after the macro
    // invocation has been processed
    node.getExpr match {
      case _ : AGeneratorExpr =>
        processMacroInvocation(node)
        node.getExpr().apply(this)
      case null =>
        processMacroInvocation(node)
      case _ =>
        node.getExpr().apply(this)
        processMacroInvocation(node)
    }

    outAMacroStmt(node)
  }
  
  
  private def processMacroInvocation(invocation : AMacroStmt) {
    // Check the arguments are type-safe
    val arguments = invocation.getArguments()
    val macroName = invocation.getIdentifier.getText
    val actualValueType = if (invocation.getExpr != null) {
      getExpressionType(invocation.getExpr())
    } else {
      null
    }
    
    // Find the macro
    val argTypes = MMap[String, Type]()
    for (parg <- arguments) {
     val arg = parg.asInstanceOf[AArgument]
      val argName = arg.getIdentifier().getText()
      argTypes(argName) = typeResolver.getType(arg.getExpr, typeNameResolver)
    }
    
    callResolver.getMatchingMacro(invocation, macroName, argTypes.toMap, typeResolver.getType(invocation.getExpr, typeNameResolver), cascadeIdentifier) match {
      case None =>
        // We should throw up an error that there are no matching macros
        val definitions = callResolver.getMacrosWithName(macroName)
        
        val callArgumentTypes = MMap[String, Type]()
        for (pargument <- arguments) {
          // Get the type of the argument in the call
          val argument = pargument.asInstanceOf[AArgument]
          val callType = typeResolver.getType(argument.getExpr(), typeNameResolver, genericsInScope)
          callArgumentTypes(argument.getIdentifier.getText) = callType
        }
        
        errors.add(new MacroNotFoundError(lineNumberProvider.getLineNumber(invocation).get, macroName, callArgumentTypes.toMap, actualValueType, definitions.toSet))
        
        // Remove this node
        invocation.replaceBy(new AErrorNode)
        
      case Some(call) =>
        // Build a map of bound generics
        val genericBindings = scala.collection.mutable.Map[GenericType, Type]()
        var isFine = true
        
        for (pargument <- arguments) {
          // Get the type of the argument in the call
          val argument = pargument.asInstanceOf[AArgument]
          val callType = typeResolver.getType(argument.getExpr(), typeNameResolver, genericsInScope)
          
          // Get the type of the argument as defined in the macro
          var defnType = call.macro.arguments(argument.getIdentifier().getText()).argType
          defnType = bind(genericBindings, defnType, callType)
          
          // Check if this type or any subtypes has been bound already
          if (defnType.isInstanceOf[GenericType] && !genericBindings.containsKey(defnType)) {
            genericBindings(defnType.asInstanceOf[GenericType]) = callType
          } else if (defnType.isInstanceOf[GenericType] && !areTypesCompatible(genericBindings.get(defnType), callType)) {
            errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(invocation).get, defnType, callType))
            isFine = false
          } else if (!areTypesCompatible(defnType, callType)) {
            errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(invocation).get, defnType, callType))
            isFine = false
          }
        }
          
        // Check if the view's controller is a match for the macro's controller
        if (call.macro.controller != null) {
          if (controllers.head == null) {
            errors.add(new IncompatibleControllerError(
                lineNumberProvider.getLineNumber(invocation).get, call.macro.name, null, call.macro.controller))
            isFine = false
          } else if (!SimpleType(controllers.head).canBeAssignedTo(call.macro.controller)) {
            errors.add(new IncompatibleControllerError(
                lineNumberProvider.getLineNumber(invocation).get, call.macro.name, SimpleType(controllers.head), call.macro.controller))
            isFine = false
          }
        }
        
        if (isFine) {
          resolvedCallsProvider.add(invocation, call)
        } else {
          // Remove this node
          invocation.replaceBy(new AErrorNode)
        }
    }
  }
  
  def bind(genericBindings : scala.collection.mutable.Map[GenericType, Type], defnType : Type, callType : Type) : Type = {
    if (defnType.isInstanceOf[GenericType] && !genericBindings.containsKey(defnType)) {
      genericBindings.put(defnType.asInstanceOf[GenericType], callType)
    } 
    
    if (defnType.isInstanceOf[GenericType] && genericBindings.containsKey(defnType)) {
      return genericBindings.get(defnType)
    }
    
    if (defnType.isInstanceOf[CompoundType]) {
      val compoundDefnType = defnType.asInstanceOf[CompoundType]
      val compoundCallType = callType.asInstanceOf[CompoundType]
      
      val innerTypes = scala.collection.mutable.ListBuffer[Type]()
      for (i <- 0 until compoundDefnType.innerTypes.size) {
        innerTypes.add(bind(genericBindings, compoundDefnType.innerTypes(i), compoundCallType.innerTypes(i)))
      }
      return new CompoundType(defnType.asInstanceOf[CompoundType].clazz, innerTypes.toList)
    } else {
      return defnType
    }
  }
  
  override def outAPropertyrefExpr(node : APropertyrefExpr) {
    typeResolver.addType(node, new SimpleType(classOf[PropertyReference[_]]))
  }
  
  override def outAPropertyExpr(node : APropertyExpr) {
    // Get the type from the variable
    // TODO Move this in to the resolver?
    val name = identifierListToString(node.getIdentifier)
    
      // TODO Make this handle multiple types of the same macro
    val macrosWithName = macroProvider.get(name).iterator
    if (macrosWithName.hasNext) {
      typeResolver.addType(
          node, 
          macrosWithName.next()) 
    } else {
      if (node.getIdentifier.size == 1) {
        typeResolver.addType(
            node, 
            scope.get(name).asInstanceOf[Type])
      } else {
        val ownerType = scope.get(node.getIdentifier.head.getText).asInstanceOf[Type]
        typeResolver.addType(
            node.getIdentifier.head, 
            ownerType)
        
        val returnType = evaluateCompoundProperty(node.getIdentifier.tail, ownerType.asInstanceOf[SimpleType].clazz)
        typeResolver.addType(
            node, 
            returnType)
      }
    }
  }
  
  private def replaceWithErrorNode(node : Node) {
    node.parent match {
      case _ : AView => node.replaceBy(new AErrorNode)
      case _ : AGenerator => node.replaceBy(new AErrorNode)
      case _ => replaceWithErrorNode(node.parent)
    }
  }
  
  def capitalise(s : String) = {
    s.substring(0, 1).toUpperCase() + s.substring(1)
  }
  
  private def evaluateCompoundProperty(identifiers : Iterable[TIdentifier], ownerClazz : Class[_]) : Type = {
    var currentType = ownerClazz
    var returnType : Type = null
    for (identifier <- identifiers) {
      val methodName = "get" + capitalise(identifier.getText())
      val getter = try {
        currentType.getMethod(methodName)
      } catch {
        case _ : NoSuchMethodException =>
          errors += new IdentifierNotFoundError(
              identifier.getLine, identifier.getText())
        return null
      }
      val t = getter.getGenericReturnType()
      
      if (t.isInstanceOf[ParameterizedTypeImpl]) {
        val p = t.asInstanceOf[ParameterizedTypeImpl]
        val innerTypes = scala.collection.mutable.Buffer[Type]()
        for (t <- p.getActualTypeArguments()) {
          innerTypes += new SimpleType(t.asInstanceOf[Class[_]])
        }
        returnType = new CompoundType(p.getRawType().asInstanceOf[Class[_]], innerTypes.toList)
      } else {
        returnType = new SimpleType(getter.getReturnType())
      }
      currentType = getter.getReturnType()
    }
    
    // TODO Only set currentType on last iteration
    return returnType
  }
  
  override def outAControllerPropExpr(node : AControllerPropExpr) {
    val returnType = evaluateCompoundProperty(node.getIdentifier, controllers.head)
    if (returnType != null) {
      typeResolver.addType(node, returnType)
    } else {
      node.replaceBy(new AErrorExpr)
    }
  }
  
  override def outAController(node : AController) {
    if (node.parent.isInstanceOf[AView]) {
      controllers.push(typeResolver.getType(node.getType, typeNameResolver).asInstanceOf[SimpleType].getClazz)
    }
  }
  
  
  def getTypeByName(clazzName : String) : Class[_] = {
    // Check to see if there is an import for this class name
    // We don't have to worry about periods as they won't be in the set of
    // imports anyway
    typeNameResolver.getClassByName(clazzName) match {
      case Some(clazz) => clazz
      case None => this.getClass().getClassLoader().loadClass(clazzName)
    }
  }
  
  
  override def outAInvertExpr(node : AInvertExpr) {
    // Get the type of each side
    val exprType = getExpressionType(node.getExpr())
    val boolType = new SimpleType(classOf[java.lang.Boolean])
    if (!areTypesCompatible(boolType, exprType)) {
      errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node).get, boolType, exprType))
    }
  }
  
  
  override def outAAssignmentStmt(node : AAssignmentStmt) {
    // This could be a dynamic macro assignment
    val destinationVariable = node.getIdentifier().getText()
    
    // TODO Cope with overloading
    if (macroProvider.get(destinationVariable).size > 0) {
      val macroDefinition = macroProvider.get(destinationVariable).iterator.next()
      
      if (macroDefinition != null && macroDefinition.isDynamic) {
        if (!areTypesCompatible(macroDefinition, typeResolver.getType(node.getExpr(), typeNameResolver, genericsInScope))) {
          errors.add(new DynamicMacroMismatchError(
              lineNumberProvider.getLineNumber(node).get, 
              macroDefinition.name))
          node.replaceBy(new AErrorNode)
        }
      } 
    } else {
      // Get the type of each side
      val lhsType = scope.get(destinationVariable).asInstanceOf[Type]
      val rhsType = getExpressionType(node.getExpr())
      
      if (!areTypesCompatible(lhsType, rhsType)) {
        errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node).get, lhsType, rhsType))
      }
    }
  }
  
  
  override def caseAView(node : AView) {
    inAView(node)

    node.getImport.foreach(_.apply(this))

    if(node.getController != null) {
      node.getController().apply(this)
    }

    node.getDefn.foreach(_.apply(this))

    node.getStmt.foreach(casePStmt(_))

    outAView(node)
  }
  
  
  override def caseAGenerator(node : AGenerator) {
    inAGenerator(node)
    node.getArgDefn.foreach(_.apply(this))
    node.getStmt.foreach(casePStmt(_))
    outAGenerator(node)
  }

  override def inAGenerator(node : AGenerator) {
    scope = new Scope(scope, owners.head)
    for (parg <- node.getArgDefn) {
      val arg = parg.asInstanceOf[AArgDefn]
      scope.add(arg.getIdentifier().getText(), typeResolver.getType(arg.getType(), typeNameResolver, genericsInScope))
    }
  }
  
  override def outAGenerator(node : AGenerator) {
    scope = scope.parentScope
  }
}