package uk.co.colinhowe.glimpse;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Node {
  private final List<Node> nodes;
  private final String id;
  private final String text;
  private final Map<String, String> attributes = new HashMap<String, String>();
  
  public List<Node> getNodes() {
    return nodes;
  }
  
  
  public String getId() {
    return id;
  }
  
  
  public String getText() {
    return text;
  }


  public Node(List<Node> nodes, String id, String text) {
    super();
    this.nodes = nodes;
    this.id = id;
    this.text = text;
  }
  
  
  public void setAttribute(final String name, final String value) {
    this.attributes.put(name, value);
  }
  
  
  public Map<String, String> getAttributes() {
    return attributes;
  }
}
