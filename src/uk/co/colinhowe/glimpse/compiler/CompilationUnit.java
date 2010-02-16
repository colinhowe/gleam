package uk.co.colinhowe.glimpse.compiler;

public class CompilationUnit {
  private final String source;
  private final String viewName;
  private final String sourceName;
  
  public CompilationUnit(String viewName, String source, String sourceName) {
    super();
    this.source = source;
    this.viewName = viewName;
    this.sourceName = sourceName;
  }

  public String getSource() {
    return source;
  }

  public String getViewName() {
    return viewName;
  }
  
  public String getSourceName() {
    return sourceName;
  }
  
  
}
