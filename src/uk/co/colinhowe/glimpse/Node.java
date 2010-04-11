package uk.co.colinhowe.glimpse;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Node {
  private final List<Node> nodes;
  private final String tagName;
  private final Object value;
  private final Map<String, Object> attributes = new HashMap<String, Object>();
  
  public List<Node> getNodes() {
    return nodes;
  }
  
  
  public String getTagName() {
    return tagName;
  }
  
  
  public Object getValue() {
    return value;
  }


  public Node(List<Node> nodes, String tagName, Object value) {
    super();
    this.nodes = nodes;
    this.tagName = tagName;
    this.value = value;
  }
  
  
  public void setAttribute(final String name, final Object value) {
    this.attributes.put(name, value);
  }
  
  
  public Map<String, Object> getAttributes() {
    return attributes;
  }
  
  public Object getAttribute(String key) {
    return attributes.get(key);
  }
}
