package uk.co.colinhowe.gleam;

import java.util.List;

import uk.co.colinhowe.gleam.infrastructure.Scope;

public interface Generator {
  public List<Node> view(Scope scope);
}
