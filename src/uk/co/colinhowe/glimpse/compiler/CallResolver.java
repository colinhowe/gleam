package uk.co.colinhowe.glimpse.compiler;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.List;


import uk.co.colinhowe.glimpse.CompilationError;
import uk.co.colinhowe.glimpse.MultipleDefinitionError;
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter;
import uk.co.colinhowe.glimpse.compiler.node.AArgDefn;
import uk.co.colinhowe.glimpse.compiler.node.AMacroDefn;
import uk.co.colinhowe.glimpse.compiler.node.PArgDefn;
import uk.co.colinhowe.glimpse.compiler.typing.Type;


public class CallResolver extends DepthFirstAdapter {
  
  private final List<CompilationError> errors = new LinkedList<CompilationError>();
  private final LineNumberProvider lineNumberProvider;
  private final TypeProvider typeProvider;
  
  public CallResolver(LineNumberProvider lineNumberProvider, TypeProvider typeProvider) {
    super();
    this.lineNumberProvider = lineNumberProvider;
    this.typeProvider = typeProvider;
  }


  public class MacroDefinition {
    private final String name;
    private final Map<String, Type> arguments;
    private final Type valueType;
    
    public MacroDefinition(final String name, final Type valueType) {
      this.name = name;
      this.valueType = valueType;
      this.arguments = new HashMap<String, Type>();
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
  }
  
  private Map<String, Set<MacroDefinition>> macros = new HashMap<String, Set<MacroDefinition>>();
  
  @Override
  public void outAMacroDefn(AMacroDefn node) {
    // Get the name of the macro
    final String name = node.getName().getText();
    
    if (!macros.containsKey(name)) {
      macros.put(name, new HashSet<MacroDefinition>());
    } else {
      errors.add(new MultipleDefinitionError(
          lineNumberProvider.getLineNumber(node), name));
    }
    
    // Get the type of the content
    Type contentType = typeProvider.getType(node.getContentType());
    final MacroDefinition defn = new MacroDefinition(name, contentType);

    // Process all the arguments
    for (PArgDefn pargDefn : node.getArgDefn()) {
      AArgDefn argDefn = (AArgDefn)pargDefn;
      Type type = typeProvider.getType(argDefn.getType());
      String argumentName = argDefn.getIdentifier().getText();
      defn.addArgument(argumentName, type);
    }
    
    
    macros.get(name).add(defn);
  }

  public Set<MacroDefinition> getMacrosWithName(final String macroName) {
    if (macros.containsKey(macroName)) {
      return macros.get(macroName);
    } else {
      return new HashSet<MacroDefinition>();
    }
  }
  
  
  public List<CompilationError> getErrors() {
    return errors;
  }
}