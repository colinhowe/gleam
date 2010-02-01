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
  private final TypeResolver typeResolver;
  
  public CallResolver(LineNumberProvider lineNumberProvider, TypeResolver typeResolver) {
    super();
    this.lineNumberProvider = lineNumberProvider;
    this.typeResolver = typeResolver;
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
    Type contentType = typeResolver.getType(node.getContentType());
    final MacroDefinition defn = new MacroDefinition(name, contentType, false);

    // Process all the arguments
    for (PArgDefn pargDefn : node.getArgDefn()) {
      AArgDefn argDefn = (AArgDefn)pargDefn;
      Type type = typeResolver.getType(argDefn.getType());
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