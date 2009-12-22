package uk.co.colinhowe.glimpse;

import java.util.List;
import java.util.Map;

public interface Generator {
  public List<Node> view(Map<String, Object> args);
}
