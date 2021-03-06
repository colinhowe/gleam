package gleam.infrastructure;

import gleam.Node;
import gleam.Macro;

public class DynamicMacro implements Macro {
  private ThreadLocal<Macro> toInvoke = new ThreadLocal<Macro>();
  
  public java.util.List<Node> invoke(Scope scope, java.util.Map<String, Object> arguments, Object value) {
    return toInvoke.get().invoke(scope, arguments, value);
  }
  
  public void setToInvoke(Macro toInvoke) {
    this.toInvoke.set(toInvoke);
  }
}
