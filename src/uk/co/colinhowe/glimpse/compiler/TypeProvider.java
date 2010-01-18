package uk.co.colinhowe.glimpse.compiler;

import java.util.HashMap;
import java.util.Map;

import uk.co.colinhowe.glimpse.Generator;
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter;
import uk.co.colinhowe.glimpse.compiler.node.AConstantExpr;
import uk.co.colinhowe.glimpse.compiler.node.AGeneratorType;
import uk.co.colinhowe.glimpse.compiler.node.AIntType;
import uk.co.colinhowe.glimpse.compiler.node.AMacroDefn;
import uk.co.colinhowe.glimpse.compiler.node.AStringExpr;
import uk.co.colinhowe.glimpse.compiler.node.AStringType;
import uk.co.colinhowe.glimpse.compiler.node.AWithInitVarDefn;
import uk.co.colinhowe.glimpse.compiler.node.Node;
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType;
import uk.co.colinhowe.glimpse.compiler.typing.Type;


public class TypeProvider extends DepthFirstAdapter {

  private final Map<Node, Type> types = new HashMap<Node, Type>();
  
  
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
}