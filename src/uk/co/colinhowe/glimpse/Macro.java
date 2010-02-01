package uk.co.colinhowe.glimpse;

import java.util.List;
import java.util.Map;

import uk.co.colinhowe.glimpse.infrastructure.Scope;

public interface Macro {
  public List<Node> invoke(Scope scope, Map<String, Object> arguments, Object value);
}
