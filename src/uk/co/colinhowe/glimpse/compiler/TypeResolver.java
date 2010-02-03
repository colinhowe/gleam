package uk.co.colinhowe.glimpse.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import uk.co.colinhowe.glimpse.Generator;
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter;
import uk.co.colinhowe.glimpse.compiler.node.ACompoundQualifiedType;
import uk.co.colinhowe.glimpse.compiler.node.ACompoundType;
import uk.co.colinhowe.glimpse.compiler.node.AConstantExpr;
import uk.co.colinhowe.glimpse.compiler.node.AGeneratorType;
import uk.co.colinhowe.glimpse.compiler.node.AGenericDefn;
import uk.co.colinhowe.glimpse.compiler.node.AIntType;
import uk.co.colinhowe.glimpse.compiler.node.AMacroDefn;
import uk.co.colinhowe.glimpse.compiler.node.APropertyExpr;
import uk.co.colinhowe.glimpse.compiler.node.AQualifiedType;
import uk.co.colinhowe.glimpse.compiler.node.ASimpleName;
import uk.co.colinhowe.glimpse.compiler.node.ASimpleQualifiedType;
import uk.co.colinhowe.glimpse.compiler.node.AStringExpr;
import uk.co.colinhowe.glimpse.compiler.node.AStringType;
import uk.co.colinhowe.glimpse.compiler.node.AWithInitVarDefn;
import uk.co.colinhowe.glimpse.compiler.node.Node;
import uk.co.colinhowe.glimpse.compiler.node.PQualifiedType;
import uk.co.colinhowe.glimpse.compiler.node.PType;
import uk.co.colinhowe.glimpse.compiler.typing.CompoundType;
import uk.co.colinhowe.glimpse.compiler.typing.GenericType;
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType;
import uk.co.colinhowe.glimpse.compiler.typing.Type;


public class TypeResolver extends DepthFirstAdapter {

  private final Map<Node, Type> types = new HashMap<Node, Type>();
  private final Map<String, Type> genericsInScope = new HashMap<String, Type>();
  private final TypeProvider typeProvider;
  private final MacroDefinitionProvider macroProvider;
  
  public void addType(Node node, Type type) {
    types.put(node, type);
  }
  
  public TypeResolver(TypeProvider typeProvider, MacroDefinitionProvider macroProvider) {
    this.typeProvider = typeProvider;
    this.macroProvider = macroProvider;
  }
  
  public Type getType(Node node) {
    if (types.containsKey(node)) {
      return types.get(node);
    } else {
      return typeProvider.get(node);
    }
  }
  
  
  @SuppressWarnings("unchecked")
  @Override
  public void outAPropertyExpr(APropertyExpr node) {
    if (node.getName() instanceof ASimpleName) {
      ASimpleName simpleName = (ASimpleName)node.getName();
      
      String name = simpleName.getIdentifier().getText();
      
      // TODO Support macro overloading
      if (macroProvider.get(name).size() > 0) {
        this.types.put(node, ((Iterator<MacroDefinition>)macroProvider.get(name).iterator()).next());
      } else {
        // TODO Get the type out
      }
    } else {
      throw new RuntimeException("Unsupported type of node");
    }
    
    // Leave the property on the stack for the next instruction to pick up

    // TODO only convert to a string at the top level expression
    // Probably use some cunning type conversion
    
    // TODO This is horrific. We should be type converting at last second.
    // Have an integer on the stack so convert it
//    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "toString", "()Ljava/lang/String;");
  }
}