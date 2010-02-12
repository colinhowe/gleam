package uk.co.colinhowe.glimpse.compiler
import uk.co.colinhowe.glimpse.IdentifierNotFoundException

import java.lang.reflect.Method
import java.util.HashMap
import java.util.Iterator
import java.util.LinkedList
import java.util.List

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
import uk.co.colinhowe.glimpse.PropertyReference


class TypeChecker(
    val lineNumberProvider : LineNumberProvider,
    val macroProvider : MacroDefinitionProvider,
    val typeResolver : TypeResolver
  ) extends DepthFirstAdapter {
  
  /*
   * TODO I think this could all be made a lot tidier
   * 
   * op(!, expects=boolean, returns=boolean)
   * op(++, expects=integer, returns=integer)
   * op(propertyReference, returns=typeOf(property))
   */
  val errors = scala.collection.mutable.Buffer[CompilationError]()
  val genericsInScope = scala.collection.mutable.Map[String,Type]()
  val imports = scala.collection.mutable.Map[String, Class[_]]()
  
  var scope : Scope = new Scope(null, false)
  var typeOnStack : Type = null
  var controllerClazz : Class[_] = null

  def getQualifiedType(ptype : PType) : Type = {
    ptype match {
      case _ : AIntType => new SimpleType(classOf[java.lang.Integer])
      case _ : AStringType => new SimpleType(classOf[java.lang.String])
      case _ : ABoolType => new SimpleType(classOf[java.lang.Boolean])
      case qualified : AQualifiedType => new SimpleType(this.getClass.getClassLoader.loadClass(nameToString(qualified)))
      case _ => throw new IllegalArgumentException("Cannot handle type [" + ptype + " : " + ptype.getClass + "]")
    }
  }
  
  def nameToString(node : AQualifiedType) : String = {
    node.toString.trim.replaceAll(" ", ".")
  }
  
  def getExpressionType(expr : PExpr) : Type = {
    expr match {
      case _ : AStringExpr => new SimpleType(classOf[java.lang.String])
      case _ : AConstantExpr => new SimpleType(classOf[java.lang.Integer])
      case _ : AFalseExpr | 
           _ : ATrueExpr 
             => new SimpleType(classOf[java.lang.Boolean])
      case _ : AInvertExpr => new SimpleType(classOf[java.lang.Boolean])
      case _ : APropertyExpr => typeResolver.getType(expr, Map[String, Type]())
      case _ : AGeneratorExpr => new SimpleType(classOf[Generator])
      case expr : AControllerPropExpr => typeResolver.getType(expr, Map[String, Type]())
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
  
  override def outAVarDefn(node: AVarDefn) {
    // Get the RHS type
    val rhsType = getExpressionType(node.getExpr())
    val varName = node.getIdentifier().getText()
    scope.add(varName, rhsType)
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
    val invocation = node.getMacroInvoke.asInstanceOf[AMacroInvoke]

    // Check the arguments are type-safe
    val arguments = invocation.getArguments()
    val macroName = invocation.getIdentifier.getText
    val actualValueType = getExpressionType(invocation.getExpr())
    
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
  
  override def outAPropertyrefExpr(node : APropertyrefExpr) {
    typeResolver.addType(node, new SimpleType(classOf[PropertyReference[_]]))
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
    } else if (pname.isInstanceOf[AQualifiedName]) {
      val qualifiedName = pname.asInstanceOf[AQualifiedName]
      val ownerType = scope.get(qualifiedName.getIdentifier().getText()).asInstanceOf[Type]
      typeResolver.addType(
          qualifiedName.getIdentifier(), 
          ownerType)
      
      val returnType = evaluateCompoundProperty(qualifiedName.getName(), ownerType.asInstanceOf[SimpleType].clazz)
      println("Compound return type [" + returnType + "]")
      typeResolver.addType(
          node, 
          returnType)
    }
  }
  
  def capitalise(s : String) = {
    s.substring(0, 1).toUpperCase() + s.substring(1)
  }
  
  private def evaluateCompoundProperty(nameNode_ : PName, ownerClazz : Class[_]) : Type = {
    var nameNode = nameNode_
    var currentType = ownerClazz
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
        returnType = new SimpleType(getter.getReturnType())
      }
      currentType = getter.getReturnType()
    }
    
    // TODO Only set currentType on last iteration
    return returnType
  }
  
  override def outAControllerPropExpr(node : AControllerPropExpr) { 
    val returnType = evaluateCompoundProperty(node.getName(), controllerClazz)
    typeResolver.addType(node, returnType)
  }
  
  override def outAController(node : AController) {
    val clazzName = nameToString(node.getName())
    controllerClazz = getTypeByName(clazzName)
  }
  
  override def outAImport(node : AImport) {
    val qualifiedName = nameToString(node.getName())
    val clazzName = nameToClazzName(node.getName())
    
    imports(clazzName) = this.getClass().getClassLoader().loadClass(qualifiedName)
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
  
  
  override def outAInvertExpr(node : AInvertExpr) {
    // Get the type of each side
    val exprType = getExpressionType(node.getExpr())
    val boolType = new SimpleType(classOf[java.lang.Boolean])
    if (!areTypesCompatible(boolType, exprType)) {
      errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node), boolType, exprType))
    }
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
    } else {
      // Get the type of each side
      val lhsType = scope.get(destinationVariable).asInstanceOf[Type]
      val rhsType = getExpressionType(node.getExpr())
      
      if (!areTypesCompatible(lhsType, rhsType)) {
        errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node), lhsType, rhsType))
      }
    }
  }
  
  def getErrors : java.util.Collection[_ <: CompilationError] = {
    JavaConversions.asList(errors)
  }
}