package uk.co.colinhowe.gleam;

import java.util.List;
import java.util.Map;

import uk.co.colinhowe.gleam.infrastructure.Scope;

public interface Macro {
  public List<Node> invoke(Scope scope, Map<String, Object> arguments, Object value);
}
