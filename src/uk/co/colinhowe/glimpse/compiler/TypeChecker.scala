package uk.co.colinhowe.glimpse.compiler
import uk.co.colinhowe.glimpse.IdentifierNotFoundException

import java.lang.reflect.Method
import java.util.HashMap
import java.util.Iterator
import java.util.LinkedList
import java.util.List
import java.util.Map

import scala.collection.JavaConversions
import scala.collection.JavaConversions._
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl
import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.DynamicMacroMismatchError
import uk.co.colinhowe.glimpse.Generator
import uk.co.colinhowe.glimpse.IdentifierNotFoundError
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


class TypeChecker(
    val lineNumberProvider : LineNumberProvider,
    val macroProvider : MacroDefinitionProvider,
    val typeResolver : TypeResolver
  ) extends DepthFirstAdapter {
  
  val errors = scala.collection.mutable.Buffer[CompilationError]()
  val genericsInScope = scala.collection.mutable.Map[String,Type]()
  
  var scope : Scope = new Scope(null, false)
  var typeOnStack : Type = null
  var controllerClazz : Class[_] = null

  def getQualifiedType(ptype : PType) : Type = {
    ptype match {
      case _ : AIntType => new SimpleType(classOf[java.lang.Integer])
      case _ : AStringType => new SimpleType(classOf[java.lang.String])
      case _ : ABoolType => new SimpleType(classOf[java.lang.Boolean])
      case _ => throw new IllegalArgumentException("Cannot handle type [" + ptype + "]")
    }
  }
  
  
  def getExpressionType(expr : PExpr) : Type = {
    expr match {
      case _ : AStringExpr => new SimpleType(classOf[java.lang.String])
      case _ : AConstantExpr => new SimpleType(classOf[java.lang.Integer])
      case _ : AFalseExpr | 
           _ : ATrueExpr 
             => new SimpleType(classOf[java.lang.Boolean])
      case _ => throw new IllegalArgumentException("Cannot handle expression[" + expr + "]")
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
    System.out.print(destinationType + " == " + sourceType + " -> ")
    System.out.println(destinationType.equals(sourceType))

    if (destinationType.isInstanceOf[MacroDefinition] && sourceType.isInstanceOf[MacroDefinition]) {
      return (destinationType.asInstanceOf[MacroDefinition]).areCompatible(sourceType.asInstanceOf[MacroDefinition])
    }
    
    if (destinationType.isInstanceOf[CompoundType] && sourceType.isInstanceOf[CompoundType]) {
      val cdt = destinationType.asInstanceOf[CompoundType]
      val sdt = sourceType.asInstanceOf[CompoundType]
      return sdt.clazz.isAssignableFrom(cdt.clazz)
    }
    
    return destinationType.equals(sourceType)
  }
  
  override def outAIncrementStmt(node : AIncrementStmt) {
    // Check that the variable is indeed an integer
    try {
      val t = scope.get(node.getIdentifier().getText()).asInstanceOf[Type]
      if (!t.equals(new SimpleType(classOf[java.lang.Integer]))) {
        errors += new TypeCheckError(lineNumberProvider.getLineNumber(node), new SimpleType(classOf[java.lang.Integer]), t)
      }
    } catch {
      case _ : IdentifierNotFoundException =>
        errors += new IdentifierNotFoundError(lineNumberProvider.getLineNumber(node), node.getIdentifier().getText())
    }
  }
  
  override def outAVarDefnStmt(node : AVarDefnStmt) {
    // Get the type of the LHS
    var varType : Type = null
    var varName : String = ""
    
    if (node.getVarDefn().isInstanceOf[AWithInitVarDefn]) {
      val defn = node.getVarDefn().asInstanceOf[AWithInitVarDefn]
      varType = getQualifiedType(defn.getType())
      varName = defn.getIdentifier().getText()
      
      // Check the RHS type
      val rhsType = getExpressionType(defn.getExpr())

      // Check if the types are compatible
      if (!areTypesCompatible(varType, rhsType)) {
        errors += new TypeCheckError(
            lineNumberProvider.getLineNumber(defn), varType, rhsType)
      }
      
      // Add the variable and the type of it to the current scope
    } else {
      val defn = node.getVarDefn().asInstanceOf[ANoInitVarDefn]
      varType = getQualifiedType(defn.getType())
      varName = defn.getIdentifier().getText()
    }
    scope.add(varName, varType)
  }
  
  implicit def toImmutableMap[K, V](mutable : scala.collection.Map[K, V]) = scala.collection.immutable.Map[K, V]() ++ mutable
  
  override def inAGenerator(node : AGenerator) {
    scope = new Scope(scope, scope.isMacroScope)
    for (parg <- node.getArgDefn) {
      val arg = parg.asInstanceOf[AArgDefn]
      scope.add(arg.getIdentifier().getText(), typeResolver.getType(arg.getType(), genericsInScope))
    }
  }
  
  override def outAGenerator(node : AGenerator) {
    scope = scope.parentScope
  }
  
  override def inAForloop(node : AForloop) {
    scope = new Scope(scope, scope.isMacroScope)
    scope.add(node.getIdentifier().getText(), typeResolver.getType(node.getType(), genericsInScope))
  }
  
  override def outAForloop(node : AForloop) {
    scope = scope.parentScope
  }
  
  override def inAMacroDefn(node : AMacroDefn) {
    scope = new Scope(scope, true)
    
    for (pdefn <- node.getGenericDefn()) {
      val defn = pdefn.asInstanceOf[AGenericDefn]
      val nodeType = typeResolver.getType(defn, genericsInScope)

      // Put this generic in scope
      System.out.println("Put generic["+defn.getIdentifier().getText()+"] in scope")
      genericsInScope.put(defn.getIdentifier().getText(), nodeType)
    }
    
    for (parg <- node.getArgDefn()) {
      val arg = parg.asInstanceOf[AArgDefn]
      scope.add(arg.getIdentifier().getText(), typeResolver.getType(arg.getType(), genericsInScope))
    }
    
    scope.add(node.getContentName().getText(), typeResolver.getType(node.getContentType(), genericsInScope))
  }
  
  override def outAMacroDefn(node : AMacroDefn) {
    scope = scope.parentScope

    // Clear any generics in scope
    genericsInScope.clear()
    System.out.println("Cleared generics from scope")
  }
  
  
  override def outAMacroStmt(node : AMacroStmt) {
    // Find the macro
    val invocation = node.getMacroInvoke

    // Check the arguments are type-safe
    var arguments : List[PArgument] = null
    var macroName : String = null
    var actualValueType : Type = null
    if (invocation.isInstanceOf[AWithStringMacroInvoke]) {
      arguments = invocation.asInstanceOf[AWithStringMacroInvoke].getArguments()
      macroName = invocation.asInstanceOf[AWithStringMacroInvoke].getIdentifier().getText()
      actualValueType = new SimpleType(classOf[java.lang.String])
    } else {
      arguments = invocation.asInstanceOf[AWithGeneratorMacroInvoke].getArguments()
      macroName = invocation.asInstanceOf[AWithGeneratorMacroInvoke].getIdentifier().getText()
      actualValueType = new SimpleType(classOf[Generator])
    }
    
    // TODO Make this handle multiple types of the same macro
    val macrosWithName = macroProvider.get(macroName).iterator()
    if (!macrosWithName.hasNext()) {
      throw new IllegalArgumentException("Macro with name [" + macroName + "] not found")
    }
    val macroDefinition = macrosWithName.next() 
    
    if (!areTypesCompatible(macroDefinition.getValueType(), actualValueType)) {
      errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node), macroDefinition.getValueType(), actualValueType))
    }
    
    // Build a map of bound generics
    val genericBindings = scala.collection.mutable.Map[GenericType, Type]()
    
    
    for (pargument <- arguments) {
      // Get the type of the argument in the call
      val argument = pargument.asInstanceOf[AArgument]
      val callType = typeResolver.getType(argument.getExpr(), genericsInScope)
      
      // Get the type of the argument as defined in the macro
      var defnType = macroDefinition.getArguments().get(argument.getIdentifier().getText())
      defnType = bind(genericBindings, defnType, callType)
      
      // Check if this type or any subtypes has been bound already
      if (defnType.isInstanceOf[GenericType] && !genericBindings.containsKey(defnType)) {
        genericBindings(defnType.asInstanceOf[GenericType]) = callType
      } else if (defnType.isInstanceOf[GenericType] && !areTypesCompatible(genericBindings.get(defnType), callType)) {
        errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node), defnType, callType))
      } else if (!areTypesCompatible(defnType, callType)) {
        errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node), defnType, callType))
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
      return new CompoundType(defnType.asInstanceOf[CompoundType].clazz, innerTypes)
    } else {
      return defnType
    }
  }
  
  override def outAPropertyExpr(node : APropertyExpr) {
    // Get the type from the variable
    // TODO Move this in to the resolver?
    val pname = node.getName()
    if (pname.isInstanceOf[ASimpleName]) {
      // TODO Make this handle multiple types of the same macro
      val macrosWithName = macroProvider.get(pname.asInstanceOf[ASimpleName].getIdentifier().getText()).iterator()
      if (macrosWithName.hasNext()) {
        typeResolver.addType(
            node, 
            macrosWithName.next()) 
      } else {
        typeResolver.addType(
            node, 
            scope.get(pname.asInstanceOf[ASimpleName].getIdentifier().getText()).asInstanceOf[Type])
      }
    }
  }
  
  def capitalise(s : String) = {
    s.substring(0, 1).toUpperCase() + s.substring(1)
  }
  
  override def outAControllerPropExpr(node : AControllerPropExpr) { 
    var nameNode = node.getName()
    var currentType = controllerClazz
    var returnType : Type = null
    while (nameNode != null) {
      var methodName : String = null
      if (nameNode.isInstanceOf[AQualifiedName]) {
        methodName = "get" + capitalise(nameNode.asInstanceOf[AQualifiedName].getIdentifier().getText())
        nameNode = nameNode.asInstanceOf[AQualifiedName].getName()
      } else {
        methodName = "get" + capitalise(nameNode.asInstanceOf[ASimpleName].getIdentifier().getText())
        nameNode = null
      }
      val getter = currentType.getMethod(methodName)
      val t = getter.getGenericReturnType()
      
      if (t.isInstanceOf[ParameterizedTypeImpl]) {
        val p = t.asInstanceOf[ParameterizedTypeImpl]
        val innerTypes = scala.collection.mutable.Buffer[Type]()
        for (t <- p.getActualTypeArguments()) {
          innerTypes += new SimpleType(t.asInstanceOf[Class[_]])
        }
        returnType = new CompoundType(p.getRawType().asInstanceOf[Class[_]], JavaConversions.asList(innerTypes))
      } else {
        returnType = new SimpleType(currentType)
      }
      currentType = getter.getReturnType()
    }
    // TODO Only set currentType on last iteration
    typeResolver.addType(node, returnType)
  }
  
  override def outAController(node : AController) {
    val clazzName = nameToString(node.getName())
    controllerClazz = this.getClass().getClassLoader().loadClass(clazzName)
  }
  
  
  def nameToString(node : PName) : String = {
    // Chunk the name down
    var nameNode = node
    var name = ""
    
    while (nameNode != null) {
      if (nameNode.isInstanceOf[AQualifiedName]) {
        name = name + nameNode.asInstanceOf[AQualifiedName].getIdentifier().getText() + "."
        nameNode = nameNode.asInstanceOf[AQualifiedName].getName()
      } else {
        name = name + nameNode.asInstanceOf[ASimpleName].getIdentifier().getText()
        nameNode = null
      }
    }
    return name
  }
  
  
  override def outAAssignmentStmt(node : AAssignmentStmt) {
    // This could be a dynamic macro assignment
    val destinationVariable = node.getIdentifier().getText()
    
    // TODO Cope with overloading
    if (macroProvider.get(destinationVariable).size() > 0) {
      val macroDefinition = macroProvider.get(destinationVariable).iterator().next()
      
      if (macroDefinition != null && macroDefinition.isDynamic()) {
        if (!areTypesCompatible(macroDefinition, typeResolver.getType(node.getExpr(), genericsInScope))) {
          errors.add(new DynamicMacroMismatchError(
              lineNumberProvider.getLineNumber(node), 
              macroDefinition.getName()))
        }
      } 
    }
  }
  
  def getErrors : java.util.Collection[_ <: CompilationError] = {
    JavaConversions.asList(errors)
  }
}