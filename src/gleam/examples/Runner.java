package gleam.examples;

import java.io.IOException;
import java.util.List;

import gleam.Node;
import gleam.tools.GleamRunner;


public class Runner {
  public static void main(final String[] args) throws IOException {
    while (true) {
      // Run a gleam view
      final GleamRunner runner = new GleamRunner();
      final List<Node> nodes = runner.run(Runner.class.getResource("runner.gleam").getFile(), null);
      
      for (final Node node : nodes) {
        print(node);
      }
      
      System.in.read();
    }
  }
  
  
  private static void print(Node node) {
    print(node, "");
  }
  
  private static void print(Node node, String indent) {
    System.out.print(indent + node.getTagName());
    
    if (node.getValue() != null) {
      System.out.println(" \"" + node.getValue() + "\"");
    } else {
      for (Node child : node.getNodes()) {
        System.out.println("");
        print(child, indent + "  ");
      }
    }
  }
}