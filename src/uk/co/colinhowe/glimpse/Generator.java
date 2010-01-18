package uk.co.colinhowe.glimpse;

import java.util.List;

import uk.co.colinhowe.glimpse.infrastructure.Scope;

public interface Generator {
  public List<Node> view(Scope scope);
}
