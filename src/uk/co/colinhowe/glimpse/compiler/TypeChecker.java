/**
 * 
 */
package uk.co.colinhowe.glimpse.compiler;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import uk.co.colinhowe.glimpse.CompilationError;
import uk.co.colinhowe.glimpse.Generator;
import uk.co.colinhowe.glimpse.IdentifierNotFoundError;
import uk.co.colinhowe.glimpse.IdentifierNotFoundException;
import uk.co.colinhowe.glimpse.TypeCheckError;
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter;
import uk.co.colinhowe.glimpse.compiler.node.AArgument;
import uk.co.colinhowe.glimpse.compiler.node.AConstantExpr;
import uk.co.colinhowe.glimpse.compiler.node.AGenericDefn;
import uk.co.colinhowe.glimpse.compiler.node.AIncrementStmt;
import uk.co.colinhowe.glimpse.compiler.node.AIntType;
import uk.co.colinhowe.glimpse.compiler.node.AMacroStmt;
import uk.co.colinhowe.glimpse.compiler.node.ANoInitVarDefn;
import uk.co.colinhowe.glimpse.compiler.node.AStringExpr;
import uk.co.colinhowe.glimpse.compiler.node.AStringType;
import uk.co.colinhowe.glimpse.compiler.node.AVarDefnStmt;
import uk.co.colinhowe.glimpse.compiler.node.AWithGeneratorMacroInvoke;
import uk.co.colinhowe.glimpse.compiler.node.AWithInitVarDefn;
import uk.co.colinhowe.glimpse.compiler.node.AWithStringMacroInvoke;
import uk.co.colinhowe.glimpse.compiler.node.PArgument;
import uk.co.colinhowe.glimpse.compiler.node.PExpr;
import uk.co.colinhowe.glimpse.compiler.node.PMacroInvoke;
import uk.co.colinhowe.glimpse.compiler.node.PType;
import uk.co.colinhowe.glimpse.compiler.typing.GenericType;
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType;
import uk.co.colinhowe.glimpse.compiler.typing.Type;
import uk.co.colinhowe.glimpse.infrastructure.Scope;

public class TypeChecker extends DepthFirstAdapter {
  private final LineNumberProvider lineNumberProvider;
  private final List<CompilationError> errors = new LinkedList<CompilationError>();
  private final MacroDefinitionProvider macroProvider;
  private final TypeProvider typeProvider;
  
  private final Scope scope;

  public TypeChecker(final LineNumberProvider lineNumberProvider, final MacroDefinitionProvider macroProvider, final TypeProvider typeProvider) {
    this.lineNumberProvider = lineNumberProvider;
    this.macroProvider = macroProvider;
    this.typeProvider = typeProvider;
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
    } else {
      throw new IllegalArgumentException("Cannot handle type [" + type + "]");
    }
  }
  
  
  public Type getExpressionType(final PExpr expr) {
    if (expr instanceof AStringExpr) {
      return new SimpleType(String.class);
    } else if (expr instanceof AConstantExpr) {
      return new SimpleType(Integer.class);
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
    // For now, we only allow types to be the same
    System.out.println(destinationType + " == " + sourceType + " -> " + destinationType.equals(sourceType));
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
      Type callType = typeProvider.getType(argument.getExpr());
      
      // Get the type of the argument as defined in the macro
      Type defnType = macroDefinition.getArguments().get(argument.getIdentifier().getText());
      
      // Check if this type has been bound already
      if (defnType instanceof GenericType && !genericBindings.containsKey(defnType)) {
        genericBindings.put((GenericType)defnType, callType);
      } else if (defnType instanceof GenericType && !areTypesCompatible(genericBindings.get(defnType), callType)) {
        errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node), defnType, callType));
      } else if (!areTypesCompatible(defnType, callType)) {
        errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node), defnType, callType));
      }
    }
  }
}