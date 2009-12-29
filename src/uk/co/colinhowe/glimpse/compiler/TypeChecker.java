/**
 * 
 */
package uk.co.colinhowe.glimpse.compiler;

import java.util.LinkedList;
import java.util.List;

import uk.co.colinhowe.glimpse.Generator;
import uk.co.colinhowe.glimpse.TypeCheckError;
import uk.co.colinhowe.glimpse.compiler.CallResolver.MacroDefinition;
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter;
import uk.co.colinhowe.glimpse.compiler.node.AArgument;
import uk.co.colinhowe.glimpse.compiler.node.AConstantExpr;
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
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType;
import uk.co.colinhowe.glimpse.compiler.typing.Type;

public class TypeChecker extends DepthFirstAdapter {
  private final LineNumberProvider lineNumberProvider;
  private final List<TypeCheckError> errors = new LinkedList<TypeCheckError>();
  private final CallResolver callResolver;
  private final TypeProvider typeProvider;

  public TypeChecker(final LineNumberProvider lineNumberProvider, final CallResolver callResolver, final TypeProvider typeProvider) {
    this.lineNumberProvider = lineNumberProvider;
    this.callResolver = callResolver;
    this.typeProvider = typeProvider;
  }
  
  
  public List<TypeCheckError> getErrors() {
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
  
  
  public void outAVarDefnStmt(AVarDefnStmt node) {
    // Get the type of the LHS
    final Type varType;
    if (node.getVarDefn() instanceof AWithInitVarDefn) {
      AWithInitVarDefn defn = (AWithInitVarDefn)node.getVarDefn();
      varType = getQualifiedType(defn.getType());
      
      // Check the RHS type
      final Type rhsType = getExpressionType(defn.getExpr());

      // Check if the types are compatible
      if (!areTypesCompatible(varType, rhsType)) {
        errors.add(new TypeCheckError(
            lineNumberProvider.getLineNumber(defn), varType, rhsType));
      }
    } else {
      varType = getQualifiedType(((ANoInitVarDefn)node.getVarDefn()).getType());
    }
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
    final MacroDefinition macroDefinition = callResolver.getMacrosWithName(macroName).iterator().next(); 
    
    if (!areTypesCompatible(macroDefinition.getValueType(), actualValueType)) {
      errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node), macroDefinition.getValueType(), actualValueType));
    }
    
    
    for (PArgument pargument : arguments) {
      // Get the type of the argument in the call
      AArgument argument = (AArgument)pargument;
      Type callType = typeProvider.getType(argument.getExpr());
      
      // Get the type of the argument as defined in the macro
      Type defnType = macroDefinition.getArguments().get(argument.getIdentifier().getText());
      
      if (!areTypesCompatible(defnType, callType)) {
        errors.add(new TypeCheckError(lineNumberProvider.getLineNumber(node), defnType, callType));
      }
    }
    
  }
}