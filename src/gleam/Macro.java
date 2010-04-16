package gleam;

import java.util.List;
import java.util.Map;

import gleam.infrastructure.Scope;

public interface Macro {
  public List<Node> invoke(Scope scope, Map<String, Object> arguments, Object value);
}
