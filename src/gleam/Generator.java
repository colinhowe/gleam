package gleam;

import java.util.List;

import gleam.infrastructure.Scope;

public interface Generator {
  public List<Node> view(Scope scope);
}
