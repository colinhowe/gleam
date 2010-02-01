/**
 * 
 */
package uk.co.colinhowe.glimpse.compiler;

import java.util.HashMap;
import java.util.Map;

import uk.co.colinhowe.glimpse.compiler.typing.Type;

public class MacroDefinition extends Type {
  private final String name;
  private final Map<String, Type> arguments;
  private final Type valueType;
  private final boolean isDynamic;
  
  public MacroDefinition(final String name, final Type valueType, final boolean isDynamic) {
    this.name = name;
    this.valueType = valueType;
    this.arguments = new HashMap<String, Type>();
    this.isDynamic = isDynamic;
  }
  
  public void addArgument(String argumentName, Type type) {
    this.arguments.put(argumentName, type);
  }
  
  public String getName() {
    return name;
  }
  
  public Type getValueType() {
    return valueType;
  }

  public Map<String, Type> getArguments() {
    return arguments;
  }
  
  public boolean isDynamic() {
    return isDynamic;
  }
  
  public boolean areCompatible(MacroDefinition other) {
    return other.arguments.equals(arguments) && other.valueType.equals(valueType);
  }
}