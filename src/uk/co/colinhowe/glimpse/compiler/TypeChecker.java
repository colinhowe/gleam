/**
 * 
 */
package uk.co.colinhowe.glimpse.compiler;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import scala.collection.JavaConversions;
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl;
import uk.co.colinhowe.glimpse.CompilationError;
import uk.co.colinhowe.glimpse.DynamicMacroMismatchError;
import uk.co.colinhowe.glimpse.Generator;
import uk.co.colinhowe.glimpse.IdentifierNotFoundError;
import uk.co.colinhowe.glimpse.IdentifierNotFoundException;
import uk.co.colinhowe.glimpse.TypeCheckError;
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter;
import uk.co.colinhowe.glimpse.compiler.node.*;
import uk.co.colinhowe.glimpse.compiler.typing.CompoundType;
import uk.co.colinhowe.glimpse.compiler.typing.GenericType;
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType;
import uk.co.colinhowe.glimpse.compiler.typing.Type;
import uk.co.colinhowe.glimpse.infrastructure.Scope;

public class TypeChecker extends DepthFirstAdapter {
  private final LineNumberProvider lineNumberProvider;
  private final List<CompilationError> errors = new LinkedList<CompilationError>();
  private final MacroDefinitionProvider macroProvider;
  private final TypeResolver typeResolver;
  private Scope scope;
  
  private Type typeOnStack;
  private Class<?> controllerClazz;

  public TypeChecker(final LineNumberProvider lineNumberProvider, final MacroDefinitionProvider macroProvider, final TypeResolver typeResolver) {
    this.lineNumberProvider = lineNumberProvider;
    this.macroProvider = macroProvider;
    this.typeResolver = typeResolver;
    this.scope = new Scope(null, false);
  }
  
  
  public List<CompilationError> getErrors() {
    return errors;
  }
  
  
  public Type getQualifiedType(final PType type) {
    if (type instanceof AIntType) {
      return new SimpleType(Integer.class);
    } else if (type instanceof AStringType) {
      return new SimpleType(String.class);
    } else if (type instanceof ABoolType) {
      return new SimpleType(Boolean.class);
    } else {
      throw new IllegalArgumentException("Cannot handle type [" + type + "]");
    }
  }
  
  
  public Type getExpressionType(final PExpr expr) {
    if (expr instanceof AStringExpr) {
      return new SimpleType(String.class);
    } else if (expr instanceof AConstantExpr) {
      return new SimpleType(Integer.class);
    } else if (expr instanceof AFalseExpr) {
      return new SimpleType(Boolean.class);
    } else if (expr instanceof ATrueExpr) {
      return new SimpleType(Boolean.class);
    } else {
      throw new IllegalArgumentException("Cannot handle expression[" + expr + "]");
    }
  }

  
  
  /**
   * Determine if the two types are compatible (the same, easily castable or a child type).
   * 
   * @param destinationType
   * @param sourceType
   * @return
   */
  public boolean areTypesCompatible(final Type destinationType, final Type sourceType) {
    System.out.print(destinationType + " == " + sourceType + " -> ");
    System.out.println(destinationType.equals(sourceType));

    if (destinationType instanceof MacroDefinition && sourceType instanceof MacroDefinition) {
      return ((MacroDefinition)destinationType).areCompatible((MacroDefinition)sourceType);
    }
    
    if (destinationType instanceof CompoundType && sourceType instanceof CompoundType) {
      CompoundType cdt = (CompoundType)destinationType;
      CompoundType sdt = (CompoundType)sourceType;
      return sdt.clazz().isAssignableFrom(cdt.clazz());
    }
    
    return destinationType.equals(sourceType);
  }
  
  
  @Override
  public void outAIncrementStmt(AIncrementStmt node) {
    // Check that the variable is indeed an integer
    try {
      Type type = (Type)scope.get(node.getIdentifier().getText());
      if (!type.equals(new SimpleType(Integer.class))) {
        errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node), new SimpleType(Integer.class), type));
      }
    } catch (IdentifierNotFoundException e) {
      errors.add(new IdentifierNotFoundError(lineNumberProvider.getLineNumber(node), node.getIdentifier().getText()));
    }
  }
  
  public void outAVarDefnStmt(AVarDefnStmt node) {
    // Get the type of the LHS
    final Type varType;
    final String varName;
    
    if (node.getVarDefn() instanceof AWithInitVarDefn) {
      AWithInitVarDefn defn = (AWithInitVarDefn)node.getVarDefn();
      varType = getQualifiedType(defn.getType());
      varName = defn.getIdentifier().getText();
      
      // Check the RHS type
      final Type rhsType = getExpressionType(defn.getExpr());

      // Check if the types are compatible
      if (!areTypesCompatible(varType, rhsType)) {
        errors.add(new TypeCheckError(
            lineNumberProvider.getLineNumber(defn), varType, rhsType));
      }
      
      // Add the variable and the type of it to the current scope
    } else {
      ANoInitVarDefn defn = (ANoInitVarDefn)node.getVarDefn();
      varType = getQualifiedType(defn.getType());
      varName = defn.getIdentifier().getText();
    }
    scope.add(varName, varType);
  }
  
  @Override
  public void inAGenerator(AGenerator node) {
    scope = new Scope(scope, scope.isMacroScope());
    for (PArgDefn parg : node.getArgDefn()) {
      AArgDefn arg = (AArgDefn)parg;
      scope.add(arg.getIdentifier().getText(), typeResolver.getType(arg.getType()));
    }
  }
  
  @Override
  public void outAGenerator(AGenerator node) {
    scope = scope.parentScope();
  }
  
  @Override
  public void inAForloop(AForloop node) {
    scope = new Scope(scope, scope.isMacroScope());
    scope.add(node.getIdentifier().getText(), typeResolver.getType(node.getType()));
  }
  
  @Override
  public void outAForloop(AForloop node) {
    scope = scope.parentScope();
  }
  
  @Override
  public void inAMacroDefn(AMacroDefn node) {
    scope = new Scope(scope, true);
    for (PArgDefn parg : node.getArgDefn()) {
      AArgDefn arg = (AArgDefn)parg;
      scope.add(arg.getIdentifier().getText(), typeResolver.getType(arg.getType()));
    }
    
    scope.add(node.getContentName().getText(), typeResolver.getType(node.getContentType()));
  }
  
  @Override
  public void outAMacroDefn(AMacroDefn node) {
    scope = scope.parentScope();
  }
  
  
  @Override
  public void outAMacroStmt(final AMacroStmt node) {
    // Find the macro
    final PMacroInvoke invocation = node.getMacroInvoke();

    // Check the arguments are type-safe
    final List<PArgument> arguments;
    final String macroName;
    final Type actualValueType;
    if (invocation instanceof AWithStringMacroInvoke) {
      arguments = ((AWithStringMacroInvoke)invocation).getArguments();
      macroName = ((AWithStringMacroInvoke)invocation).getIdentifier().getText();
      actualValueType = new SimpleType(String.class);
    } else {
      arguments = ((AWithGeneratorMacroInvoke)invocation).getArguments();
      macroName = ((AWithGeneratorMacroInvoke)invocation).getIdentifier().getText();
      actualValueType = new SimpleType(Generator.class);
    }
    
    // TODO Make this handle multiple types of the same macro
    Iterator<MacroDefinition> macrosWithName = macroProvider.get(macroName).iterator();
    if (!macrosWithName.hasNext()) {
      throw new IllegalArgumentException("Macro with name [" + macroName + "] not found");
    }
    final MacroDefinition macroDefinition = macrosWithName.next(); 
    
    if (!areTypesCompatible(macroDefinition.getValueType(), actualValueType)) {
      errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node), macroDefinition.getValueType(), actualValueType));
    }
    
    // Build a map of bound generics
    Map<GenericType, Type> genericBindings = new HashMap<GenericType, Type>();
    
    
    for (PArgument pargument : arguments) {
      // Get the type of the argument in the call
      AArgument argument = (AArgument)pargument;
      Type callType = typeResolver.getType(argument.getExpr());
      
      // Get the type of the argument as defined in the macro
      Type defnType = macroDefinition.getArguments().get(argument.getIdentifier().getText());
      defnType = bind(genericBindings, defnType, callType);
      
      // Check if this type or any subtypes has been bound already
      if (defnType instanceof GenericType && !genericBindings.containsKey(defnType)) {
        genericBindings.put((GenericType)defnType, callType);
      } else if (defnType instanceof GenericType && !areTypesCompatible(genericBindings.get(defnType), callType)) {
        errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node), defnType, callType));
      } else if (!areTypesCompatible(defnType, callType)) {
        errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node), defnType, callType));
      }
    }
  }
  
  public Type bind(Map<GenericType, Type> genericBindings, Type defnType, Type callType) {
    if (defnType instanceof GenericType && !genericBindings.containsKey(defnType)) {
      genericBindings.put((GenericType)defnType, callType);
    } 
    
    if (defnType instanceof GenericType && genericBindings.containsKey(defnType)) {
      return genericBindings.get(defnType);
    }
    
    if (defnType instanceof CompoundType) {
      CompoundType compoundDefnType = (CompoundType)defnType;
      CompoundType compoundCallType = (CompoundType)callType;
      
      List<Type> innerTypes = new LinkedList<Type>();
      for (int i = 0; i < compoundDefnType.innerTypes().size(); i++) {
        innerTypes.add(bind(genericBindings, compoundDefnType.innerTypes().get(i), compoundCallType.innerTypes().get(i)));
      }
      return new CompoundType(((CompoundType)defnType).clazz(), innerTypes);
    } else {
      return defnType;
    }
  }
  
  public void outAPropertyExpr(APropertyExpr node) {
    // Get the type from the variable
    // TODO Move this in to the resolver?
    PName pname = node.getName();
    if (pname instanceof ASimpleName) {
      // TODO Make this handle multiple types of the same macro
      Iterator<MacroDefinition> macrosWithName = macroProvider.get(((ASimpleName)pname).getIdentifier().getText()).iterator();
      if (macrosWithName.hasNext()) {
        typeResolver.addType(
            node, 
            macrosWithName.next()); 
      } else {
        typeResolver.addType(
            node, 
            (Type)scope.get(((ASimpleName)pname).getIdentifier().getText()));
      }
    }
  }
  
  public String capitalise(String s) {
    return s.substring(0, 1).toUpperCase() + s.substring(1);
  }
  
  @Override
  public void outAControllerPropExpr(AControllerPropExpr node) { 
    PName nameNode = node.getName();
    Class<?> currentType = controllerClazz;
    Type returnType = null;
    while (nameNode != null) {
      final String methodName;
      if (nameNode instanceof AQualifiedName) {
        methodName = "get" + capitalise(((AQualifiedName)nameNode).getIdentifier().getText());
        nameNode = ((AQualifiedName)nameNode).getName();
      } else {
        methodName = "get" + capitalise(((ASimpleName)nameNode).getIdentifier().getText());
        nameNode = null;
      }
      Method getter;
      try {
        getter = currentType.getMethod(methodName);
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
      Object t = getter.getGenericReturnType();
      
      if (t instanceof ParameterizedTypeImpl) {
        ParameterizedTypeImpl p = (ParameterizedTypeImpl)t;
        List innerTypes = new LinkedList<Class<?>>();
        for (java.lang.reflect.Type type : p.getActualTypeArguments()) {
          innerTypes.add(new SimpleType((Class)type));
        }
        returnType = new CompoundType(
            (Class)p.getRawType(), 
            innerTypes);
      } else {
        returnType = new SimpleType(currentType);
      }
      currentType = getter.getReturnType();
    }
    // TODO Only set currentType on last iteration
    typeResolver.addType(node, returnType);
  }
  
  @Override
  public void outAController(AController node) {
    String clazzName = nameToString(node.getName());
    try {
      controllerClazz = this.getClass().getClassLoader().loadClass(clazzName);
    } catch (ClassNotFoundException e) {
      throw new RuntimeException(e);
    }
  }
  
  
  public String nameToString(PName node) {
    // Chunk the name down
    PName nameNode = node;
    String name = "";
    
    while (nameNode != null) {
      if (nameNode instanceof AQualifiedName) {
        name = name + ((AQualifiedName)nameNode).getIdentifier().getText() + ".";
        nameNode = ((AQualifiedName)nameNode).getName();
      } else {
        name = name + ((ASimpleName)nameNode).getIdentifier().getText();
        nameNode = null;
      }
    }
    return name;
  }
  
  
  @Override
  public void outAAssignmentStmt(AAssignmentStmt node) {
    // This could be a dynamic macro assignment
    String destinationVariable = node.getIdentifier().getText();
    
    // TODO Cope with overloading
    if (macroProvider.get(destinationVariable).size() > 0) {
      MacroDefinition macroDefinition = (MacroDefinition)macroProvider.get(destinationVariable).iterator().next();
      
      if (macroDefinition != null && macroDefinition.isDynamic()) {
        if (!areTypesCompatible(macroDefinition, typeResolver.getType(node.getExpr()))) {
          errors.add(new DynamicMacroMismatchError(
              lineNumberProvider.getLineNumber(node), 
              macroDefinition.getName()));
        }
      } 
    }
  }
}