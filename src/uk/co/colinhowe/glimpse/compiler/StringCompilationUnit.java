package uk.co.colinhowe.glimpse.compiler;

public class StringCompilationUnit extends CompilationUnit {
  private final String source;
  
  public StringCompilationUnit(String viewName, String sourceName, String source) {
    super(viewName, sourceName);
    this.source = source;
  }

  public String getSource() {
    return source;
  }
}
