package uk.co.colinhowe.glimpse.compiler;

import java.util.ArrayList;
import java.util.HashMap;
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
import uk.co.colinhowe.glimpse.compiler.node.AQualifiedType;
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


public class TypeProvider extends DepthFirstAdapter {

  private final Map<Node, Type> types = new HashMap<Node, Type>();
  private final Map<String, Type> genericsInScope = new HashMap<String, Type>();
  
  public Type getType(Node node) {
    return types.get(node);
  }
  
  
  @Override
  public void outAConstantExpr(AConstantExpr node) {
    types.put(node, new SimpleType(Integer.class));
  }
  
  @Override
  public void outAStringExpr(AStringExpr node) {
    types.put(node, new SimpleType(String.class));
  }
  
  @Override
  public void outAWithInitVarDefn(AWithInitVarDefn node) {
    types.put(node, getType(node.getType()));
  }
  
  @Override
  public void outAIntType(AIntType node) {
    types.put(node, new SimpleType(Integer.class));
  }
  
  @Override
  public void outAStringType(AStringType node) {
    types.put(node, new SimpleType(String.class));
  }
  
  @Override
  public void outAGeneratorType(AGeneratorType node) {
    types.put(node, new SimpleType(Generator.class));
  }
  
  @Override
  public void outAGenericDefn(AGenericDefn node) {
    GenericType type = new GenericType(node.getIdentifier().getText(), Object.class);
    types.put(node, type);
    
    // Put this generic in scope
    genericsInScope.put(node.getIdentifier().getText(), type);
  }
  
  @Override
  public void outAQualifiedType(AQualifiedType node) {
    // Determine the full type
    PQualifiedType typeNode = node.getQualifiedType();
    String type = "";
    
    while (typeNode != null) {
      if (typeNode instanceof ASimpleQualifiedType) {
        type += ((ASimpleQualifiedType)typeNode).getIdentifier().getText();
        typeNode = null;
      } else if (typeNode instanceof ACompoundQualifiedType) {
        type += ((ACompoundQualifiedType)typeNode).getIdentifier().getText() + ".";
        typeNode = ((ACompoundQualifiedType)typeNode).getQualifiedType();
      }
    }
    
    // Check if this is a generic type
    if (genericsInScope.containsKey(type)) {
      types.put(node, genericsInScope.get(type));
    } else {
     
      final ClassLoader loader = this.getClass().getClassLoader();
      Class<?> clazz;
      try {
        clazz = loader.loadClass(type);
      } catch (ClassNotFoundException e) {
        throw new RuntimeException(e);
      }
      types.put(node, new SimpleType(clazz));
    }
  }
  
  @Override
  public void outACompoundType(ACompoundType node) {
    SimpleType parentType = (SimpleType)types.get(node.getParenttype());
    
    List<Type> subTypes = new ArrayList<Type>();
    for (PType typeNode : node.getTypes()) {
      subTypes.add(types.get(typeNode));
    }
    
    types.put(node, new CompoundType(parentType.getClazz(), subTypes));
  }
  
//  
//  
//  @Override
//  public void outASimpleType(ASimpleType node) {
//    // Look for a matching identifier in scope
//    
//    // Look for a matching generic
//    if (genericsInScope.containsKey(node.getIdentifier().getText())) {
//      types.put(node, genericsInScope.get(node.getIdentifier().getText()));
//    }
//  }
  
  
  @Override
  public void outAMacroDefn(AMacroDefn node) {
    // Done with the macro - wipe the generics in scope
    genericsInScope.clear();
  }
}