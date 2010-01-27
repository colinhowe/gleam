package uk.co.colinhowe.glimpse;

public class MultipleDefinitionError extends CompilationError {
  private final String macroName;
  
  public String getMacroName() {
    return macroName;
  }

  public MultipleDefinitionError(int lineNumber, String macroName) {
    super(lineNumber);
    this.macroName = macroName;
  }
}
