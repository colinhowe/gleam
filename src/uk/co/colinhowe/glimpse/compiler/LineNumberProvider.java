package uk.co.colinhowe.glimpse.compiler;

import java.util.HashMap;
import java.util.Map;

import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter;
import uk.co.colinhowe.glimpse.compiler.node.Node;
import uk.co.colinhowe.glimpse.compiler.node.Token;


public class LineNumberProvider extends DepthFirstAdapter {
  
  private final Map<Node, Integer> firstLineNumbers = new HashMap<Node, Integer>();
  
  
  @Override
  public void defaultCase(Node node) {
    if (node instanceof Token) {
      Node currentParent = node.parent();
      final int line = ((Token)node).getLine();
      while (currentParent != null) {
        if (!firstLineNumbers.containsKey(currentParent)) {
          firstLineNumbers.put(currentParent, line);
        }
        currentParent = currentParent.parent();
      }
    }
    super.defaultCase(node);
  }
  
  
  public int getLineNumber(Node node) {
    return firstLineNumbers.get(node);
  }
}